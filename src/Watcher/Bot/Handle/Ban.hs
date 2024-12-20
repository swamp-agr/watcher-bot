module Watcher.Bot.Handle.Ban where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM_, void, when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Telegram.Bot.API
import Telegram.Bot.API.Names
import Telegram.Bot.Simple

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as Text

import Watcher.Bot.Analytics
import Watcher.Bot.Cache
import Watcher.Bot.Handle.Contact
import Watcher.Bot.Reply
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.State.Chat
import Watcher.Bot.Types
import Watcher.Bot.Utils

handleBanAction
  :: BotState -> ChatId -> ChatState -> VoterId -> MessageId -> MessageInfo -> BotM ()
handleBanAction model chatId ch@ChatState{..} voterId messageId orig = do
  void $ call model $ deleteMessage chatId messageId
  forwardToOwnersMaybe model Spam chatId (messageInfoId orig)
  if coerce @_ @UserId voterId `HS.member` chatAdmins
    then handleBanByAdmin model chatId orig
    else handleBanByRegularUser model chatId ch (Just voterId) orig

handleAdminBan
  :: BotState
  -> ChatId
  -> ChatState
  -> UserId
  -> MessageId
  -> AdminBanId
  -> BotM ()
handleAdminBan model@BotState{..} chatId ch@ChatState{..} userId messageId adminBanId = do
  let userIsAdmin = userId `HS.member` chatAdmins
      spamerId = adminBanIdToSpamerId adminBanId
  when userIsAdmin $ do
    case adminBanId of
      AdminAgainstBan _ _ -> do
        void $ call model $ deleteMessage chatId messageId
        let nextState = ch { adminCalls = HM.delete spamerId adminCalls }
        writeCache groups chatId nextState
        pure ()
      AdminForBan _ _ -> do
        void $ call model $ deleteMessage chatId messageId
        case HM.lookup spamerId adminCalls  of
          Nothing -> pure ()
          Just (spamer, orig) -> do
            updateBlocklistAndMessages model chatId orig
            banSpamerInChat model chatId spamer
            let nextState = ch { adminCalls = HM.delete spamerId adminCalls }
            writeCache groups chatId nextState
            selfDestructReply model chatId ch (ReplyUserAlreadyBanned spamer)

banSpamerInChat :: BotState -> ChatId -> UserInfo -> BotM ()
banSpamerInChat model chatId spamer = do
  now <- liftIO getCurrentTime
  sendEvent model (chatEvent now chatId EventGroupBan)

  let banReq = (defBanChatMember (SomeChatId chatId) (userInfoId spamer))
        { banChatMemberRevokeMessages = Just True }
  withDebug model $! replyText "banChatMember requested"
  unlessDebug model $! void $ call model $ banChatMember banReq

handleBanByAdmin :: Model -> ChatId -> MessageInfo -> BotM ()
handleBanByAdmin model chatId orig@MessageInfo{..} = do
  now <- liftIO getCurrentTime
  let evt = (chatEvent now chatId EventGroupSpam)
        { eventData = Just "by_admin" }
  sendEvent model evt

  let mSpamerUser = messageInfoFrom
  updateBlocklistAndMessages model chatId orig
  -- according to telegram it should be empty only in channels
  forM_ mSpamerUser $! banSpamerInChat model chatId

handleBanByRegularUser
  :: BotState -> ChatId -> ChatState -> Maybe VoterId -> MessageInfo -> BotM ()
handleBanByRegularUser model chatId ch@ChatState{..} mVoterId orig = do
  now <- liftIO getCurrentTime
  let evt = (chatEvent now chatId EventGroupSpam)
        { eventData = Just "by_user_or_bot" }
  sendEvent model evt

  let GroupSettings{spamCommandAction, usersForConsensus} = chatSettings
      mSpamerUser = messageInfoFrom orig
  forM_ mSpamerUser $ \spamer -> case spamCommandAction of
    SCPoll ->
      handleBanViaConsensusPoll model chatId ch mVoterId spamer orig usersForConsensus
    SCAdminsCall -> handleBanViaAdminsCall model chatId ch spamer orig

handleBanViaAdminsCall
  :: BotState -> ChatId -> ChatState -> UserInfo -> MessageInfo -> BotM ()
handleBanViaAdminsCall model@BotState{..} chatId ch@ChatState{..} spamer orig = do
  let spamerId = SpamerId $! userInfoId spamer
      nextState = ch
        { adminCalls = HM.insert spamerId (spamer, orig) adminCalls }
  writeCache groups chatId nextState

  (call model $ getChatAdministrators (SomeChatId chatId)) >>= \case
    Just Response{..} -> if not responseOk
      then liftIO (log' @Text "Cannot retrieve admins for ban call")
      else do
       let adminUsernames = makeUserLink . chatMemberUser <$> responseResult
       replyCallAdmins chatId spamerId adminUsernames orig
    -- Fallback option is enabled only for test purposes
    Nothing -> withDebug model $ (call model $ getChat (SomeChatId chatId)) >>= \case
      Nothing -> liftIO $ log' @Text "Cannot retrieve neither admins nor user for ban call"
      Just Response{..} -> do
        let adminUsernames = [makeChatLink responseResult]
        replyCallAdmins chatId spamerId adminUsernames orig

handleBanViaConsensusPoll
  :: BotState
  -> ChatId
  -> ChatState
  -> Maybe VoterId
  -> UserInfo
  -> MessageInfo
  -> Integer
  -> BotM ()
handleBanViaConsensusPoll model@BotState{..} chatId ch@ChatState{..} mVoterId spamer orig consensus = do
  userAlreadyBanned <- hasUserAlreadyBannedElsewhere model (userInfoId spamer)
  if userAlreadyBanned
    then do
      updateBlocklistAndMessages model chatId orig
      banSpamerInChat model chatId spamer
      selfDestructReply model chatId ch (ReplyUserAlreadyBanned spamer)
    else do
      botItself <- liftIO $ readTVarIO self
      let spamerId = SpamerId $! userInfoId spamer
          mVoterIdOrBotId = mVoterId <|> ((VoterId . userInfoId) <$> botItself)
          mPollState = HM.lookup spamerId activePolls

          adminReported = coerce spamerId `HS.member` chatAdmins
          selfReport = maybe False (coerce spamerId ==) mVoterId

      unless (adminReported || selfReport) $ do
        mPoll <- case mPollState of
          Nothing -> createBanPoll
            model ch chatId spamerId mVoterIdOrBotId consensus spamer
              $! messageInfoId orig
          Just poll -> do
            let currentPollSize = HS.size (pollVoters poll)
                nextPollState =
                  maybe poll
                    (\voterId -> fst $! addVoteToPoll ch voterId spamerId poll) mVoterId
                pollSizeChanged = currentPollSize /= HS.size (pollVoters nextPollState)
            pure $! Just (pollSizeChanged, nextPollState)

        -- at this point poll *must* be created
        forM_ mPoll $ \poll -> proceedWithPoll model ch chatId spamerId mVoterId (Just orig) poll

-- | This message is definitely a spam! So let's:
--
-- 1. remove it.
-- 2. add them to list of spam messages.
-- 3. add spamer to blocklist.
updateBlocklistAndMessages :: MonadIO m => BotState -> ChatId -> MessageInfo -> m ()
updateBlocklistAndMessages model@BotState{..} chatId MessageInfo{..} = do
  void $ call model $ deleteMessage (chatInfoId messageInfoChat) messageInfoId
  forM_ messageInfoText $ \txt' -> do
    let txt = MessageText txt'
    alterCache spamMessages txt (Just . succ . fromMaybe 1)
    case messageInfoFrom of
      Nothing -> pure () -- FIXME: there is a gap between spamers and messages
      Just UserInfo{userInfoId} -> updateBlocklist model chatId userInfoId (Just txt)

updateBlocklist :: MonadIO m => BotState -> ChatId -> UserId -> Maybe MessageText -> m ()
updateBlocklist BotState {..} chatId userId mMessage =
  let go Nothing = Just $! newBanState
        { bannedMessages = maybe HS.empty HS.singleton mMessage
        , bannedChats = HS.singleton chatId
        }
      go (Just hs@BanState{..}) =
        Just $! hs
          { bannedMessages = maybe bannedMessages (flip HS.insert bannedMessages) mMessage
          , bannedChats = HS.insert chatId bannedChats
          }
  in alterCache blocklist userId go

hasUserAlreadyBannedElsewhere :: BotState -> UserId -> BotM Bool
hasUserAlreadyBannedElsewhere BotState{..} userId =
  lookupCache blocklist userId >>= pure . isJust

proceedWithPoll
  :: BotState
  -> ChatState
  -> ChatId
  -> SpamerId
  -> Maybe VoterId
  -> Maybe MessageInfo
  -> (Bool, PollState)
  -> BotM ()
proceedWithPoll model ch@ChatState{..} chatId spamerId voterId mOrig (pollChanged, poll@PollState{..}) = do
  let consensus = usersForConsensus chatSettings
      voters = HS.size pollVoters
      checkVoterId = (`HS.member` chatAdmins) . coerce @_ @UserId
      enoughToBan = fromIntegral consensus <= voters || maybe False checkVoterId voterId
  case (not pollChanged, enoughToBan) of
    -- Poll message has been originated, nothing to worry about
    (True, _) -> pure ()
    (False, False) ->
      updateBanPoll model ch chatId spamerId voters consensus poll
    (_, True)  -> do
      forM_ mOrig $! updateBlocklistAndMessages model chatId
      unlessDebug model $! banSpamerInChat model chatId pollSpamer
      closeBanPoll model ch chatId spamerId
      void $ call model $ deleteMessage chatId pollSpamMessageId
      void $ call model $ deleteMessage chatId pollMessageId
      selfDestructReply model chatId ch (ReplyConsensus voters)

handleVoteBan
  :: BotState
  -> ChatId
  -> ChatState
  -> VoterId
  -> MessageId -- it comes from callback, worth take a look at it
  -> VoteBanId
  -> BotM ()
handleVoteBan model chatId ch@ChatState{..} voterId _messageId voteBanId = do
  liftIO $ log' @Text "handleVoteBan"
  let spamerId = voteBanIdToSpamerId voteBanId
      selfVote = coerce @SpamerId @UserId spamerId == coerce @VoterId @UserId voterId
  liftIO $ log' (selfVote, not (isDebugEnabled model))
  -- normally we do not want to vote for/against self
  -- but it is needed for debug only
  if selfVote && not (isDebugEnabled model)
    then pure ()
    else case HM.lookup spamerId activePolls of
      -- Race? Poll's been closed but decision was too late? Don't care
      Nothing -> pure ()
      Just poll@PollState{pollMessageId, pollSpamer} -> case voteBanId of
        -- One independent voice is enough to close the poll
        VoteAgainstBan _ _ -> do
          closeBanPoll model ch chatId spamerId
          void $ call model $ deleteMessage chatId pollMessageId
          selfDestructReply model chatId ch (ReplyUserRecovered pollSpamer)
        VoteForBan _ _ -> do
          let (newPoll, nextChatState) = addVoteToPoll ch voterId spamerId poll
              pollChanged = HS.size (pollVoters poll) /= HS.size (pollVoters newPoll)
          proceedWithPoll model nextChatState chatId spamerId (Just voterId) Nothing (pollChanged, newPoll)

closeBanPoll :: BotState -> ChatState -> ChatId -> SpamerId -> BotM ()
closeBanPoll BotState{..} st@ChatState{..} chatId spamerId = do
  let nextChatState = st { activePolls = HM.delete spamerId activePolls }
  writeCache groups chatId nextChatState

allowUserInGroup :: BotState -> ChatState -> ChatId -> UserId -> BotM ()
allowUserInGroup BotState{..} st@ChatState{..} chatId userId = do
  let nextChatState = st { allowlist = HS.insert userId allowlist }
  writeCache groups chatId nextChatState

createBanPoll
  :: BotState
  -> ChatState
  -> ChatId
  -> SpamerId
  -> Maybe VoterId
  -> Integer -- votes required for ban decision
  -> UserInfo -- spamer
  -> MessageId
  -> BotM (Maybe (Bool, PollState))
createBanPoll model@BotState{..} st chatId spamerId mVoterId consensus spamer messageId = do
  let keyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = voteButtons chatId spamerId
        }
      replyMsg = (toReplyMessage (voteMessage (maybe 0 (const 1) mVoterId) consensus))
        { replyMessageReplyToMessageId = Just messageId
        , replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup keyboard
        }
      req = replyMessageToSendMessageRequest (SomeChatId chatId) replyMsg
  mResponse <- call model $ sendMessage req
  case mResponse of
    Nothing -> pure Nothing
    Just Response{..} -> if not responseOk
      then pure Nothing
      else do
        let pollId = messageMessageId responseResult
            (poll, nextChatState) = startBanPoll st mVoterId spamerId spamer pollId messageId
        writeCache groups chatId nextChatState
        pure $ Just (True, poll)

updateBanPoll
  :: BotState
  -> ChatState
  -> ChatId
  -> SpamerId
  -> Int
  -> Integer
  -> PollState
  -> BotM ()
updateBanPoll
  BotState{..} st@ChatState{..} chatId spamerId voters consensus poll@PollState{..} = do
  let nextChatState = st { activePolls = HM.insert spamerId poll activePolls }
  writeCache groups chatId nextChatState

  let keyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = voteButtons chatId spamerId
        }
      editMsgTxt = voteMessage voters consensus
      editMsg = (toEditMessage editMsgTxt)
        { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup keyboard
        }
      editId = EditChatMessageId (SomeChatId chatId) pollMessageId

  editMessage editId editMsg

voteMessage :: Int -> Integer -> Text
voteMessage voters consensus = Text.concat
  [ "Someone decided that this is a spamer. Is it correct? Vote ("
  , s2t voters , "/", s2t consensus, ")"
  ]

voteButtons :: ChatId -> SpamerId -> [[InlineKeyboardButton]]
voteButtons chatId spamerId =
   [ makeButton <$> [ ("Yes", VoteForBan chatId spamerId), ("No", VoteAgainstBan chatId spamerId) ] ]
  where
    makeButton = uncurry actionButton

handleBotBanAction
  :: BotState -> ChatId -> ChatState -> UserId -> ChatMember -> BotM ()
handleBotBanAction model@BotState{..} chatId ChatState{..} botUserId bannedMember = do
  let botIsAdmin' = botUserId `HS.member` chatAdmins
      spamerId = userId $ chatMemberUser bannedMember
      go = Just . fromMaybe newBanState

  when botIsAdmin' $ do
    now <- liftIO getCurrentTime
    let evt = (chatEvent now chatId EventGroupSpam)
          { eventData = Just "by_other_admin_bot" }
    sendEvent model evt

    alterCache blocklist spamerId go
