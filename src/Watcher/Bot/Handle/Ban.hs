module Watcher.Bot.Handle.Ban where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM_, void, when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Telegram.Bot.API
import Telegram.Bot.API.Names
import Telegram.Bot.Simple

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Set as Set
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
  :: WithBotState => ChatId -> ChatState -> VoterId -> MessageId -> MessageInfo -> BotM ()
handleBanAction chatId ch@ChatState{..} voterId messageId orig = do
  void $ call $ deleteMessage chatId messageId
  forwardToOwnersMaybe Spam chatId (messageInfoId orig)
  if coerce @_ @UserId voterId `HS.member` chatAdmins
    then handleBanByAdmin chatId ch orig
    else handleBanByRegularUser chatId ch (Just voterId) orig

handleAdminBan
  :: WithBotState
  => ChatId
  -> ChatState
  -> UserId
  -> MessageId
  -> AdminBanId
  -> BotM ()
handleAdminBan chatId ch@ChatState{..} userId messageId adminBanId = do
  let BotState{..} = ?model
      userIsAdmin = userId `HS.member` chatAdmins
      spamerId = adminBanIdToSpamerId adminBanId
  when userIsAdmin $ do
    case adminBanId of
      AdminAgainstBan _ _ -> do
        void $ call $ deleteMessage chatId messageId
        let nextState = ch { adminCalls = HM.delete spamerId adminCalls }
        writeCache groups chatId nextState
        pure ()
      AdminForBan _ _ -> do
        void $ call $ deleteMessage chatId messageId
        case HM.lookup spamerId adminCalls  of
          Nothing -> pure ()
          Just (spamer, orig) -> do
            updateBlocklistAndMessages chatId orig
            banSpamerInChat chatId spamer
            removeAllQuarantineMessages ch chatId spamerId
            let nextState =
                  ch { adminCalls = HM.delete spamerId adminCalls
                     , quarantine = HM.delete (coerce @_ @UserId spamerId) quarantine
                     }
            writeCache groups chatId nextState
            selfDestructReply chatId ch (ReplyUserAlreadyBanned spamer)

banSpamerInChat :: WithBotState => ChatId -> UserInfo -> BotM ()
banSpamerInChat chatId spamer = do
  now <- liftIO getCurrentTime
  sendEvent (chatEvent now chatId EventGroupBan)

  let banReq = (defBanChatMember (SomeChatId chatId) (userInfoId spamer))
        { banChatMemberRevokeMessages = Just True }
  withDebug $! replyText "banChatMember requested"
  unlessDebug $! void $ call $ banChatMember banReq

handleBanByAdmin :: WithBotState => ChatId -> ChatState -> MessageInfo -> BotM ()
handleBanByAdmin chatId ch@ChatState{..} orig@MessageInfo{..} = do
  now <- liftIO getCurrentTime
  let evt = (chatEvent now chatId EventGroupSpam)
        { eventData = Just "by_admin" }
  sendEvent evt

  let mSpamerUser = messageInfoFrom
  updateBlocklistAndMessages chatId orig

  -- according to telegram it should be empty only in channels
  forM_ mSpamerUser \spamer -> do
    -- remove poll if exists
    let spamerId = SpamerId $ userInfoId spamer
        mPoll = HM.lookup spamerId activePolls
    forM_ (pollMessageId <$> mPoll) (void . call . deleteMessage chatId)
    closeBanPoll ch chatId spamerId

    -- remove all messages tracked in quarantine
    removeAllQuarantineMessages ch chatId spamerId
    
    banSpamerInChat chatId spamer

handleBanByRegularUser
  :: WithBotState => ChatId -> ChatState -> Maybe VoterId -> MessageInfo -> BotM ()
handleBanByRegularUser chatId ch@ChatState{..} mVoterId orig = do
  now <- liftIO getCurrentTime
  let evt = (chatEvent now chatId EventGroupSpam)
        { eventData = Just "by_user_or_bot" }
  sendEvent evt

  let GroupSettings{spamCommandAction, usersForConsensus} = chatSettings
      mSpamerUser = messageInfoFrom orig
  forM_ mSpamerUser $ \spamer -> case spamCommandAction of
    SCPoll ->
      handleBanViaConsensusPoll chatId ch mVoterId spamer orig usersForConsensus
    SCAdminsCall -> handleBanViaAdminsCall chatId ch spamer orig

handleBanViaAdminsCall
  :: WithBotState => ChatId -> ChatState -> UserInfo -> MessageInfo -> BotM ()
handleBanViaAdminsCall chatId ch@ChatState{..} spamer orig = do
  let BotState {..} = ?model
      spamerId = SpamerId $! userInfoId spamer
      nextState = ch
        { adminCalls = HM.insert spamerId (spamer, orig) adminCalls }
  writeCache groups chatId nextState

  (call $ getChatAdministrators (SomeChatId chatId)) >>= \case
    Just Response{..} -> if not responseOk
      then liftIO (logT "Cannot retrieve admins for ban call")
      else do
       let adminUsernames = makeUserLink . chatMemberUser <$> responseResult
       replyCallAdmins chatId spamerId adminUsernames orig
    -- Fallback option is enabled only for test purposes
    Nothing -> withDebug $ (call $ getChat (SomeChatId chatId)) >>= \case
      Nothing -> liftIO $ logT "Cannot retrieve neither admins nor user for ban call"
      Just Response{..} -> do
        let adminUsernames = [makeChatLink responseResult]
        replyCallAdmins chatId spamerId adminUsernames orig

handleBanViaConsensusPoll
  :: WithBotState
  => ChatId
  -> ChatState
  -> Maybe VoterId
  -> UserInfo
  -> MessageInfo
  -> Integer
  -> BotM ()
handleBanViaConsensusPoll chatId ch@ChatState{..} mVoterId spamer orig consensus = do
  let BotState {..} = ?model
      spamerId = SpamerId $! userInfoId spamer
  userAlreadyBanned <- hasUserAlreadyBannedElsewhere (userInfoId spamer)
  if userAlreadyBanned
    then do
      updateBlocklistAndMessages chatId orig
      banSpamerInChat chatId spamer
      removeAllQuarantineMessages ch chatId spamerId
      selfDestructReply chatId ch (ReplyUserAlreadyBanned spamer)
    else do
      botItself <- liftIO $ readTVarIO self
      let mBotId = (VoterId . userInfoId) <$> botItself
          mVoterIdOrBotId = mVoterId <|> mBotId
          voter = if isNothing mVoterIdOrBotId || isJust mBotId
            then BotVoter
            else UserVoter
          mPollState = HM.lookup spamerId activePolls

          adminReported = coerce spamerId `HS.member` chatAdmins
          selfReport = maybe False (coerce spamerId ==) mVoterId

      unless (adminReported || selfReport) $ do
        mPoll <- case mPollState of
          Nothing -> createBanPoll ch chatId spamerId mVoterIdOrBotId voter consensus spamer
            $! messageInfoId orig
          Just poll -> do
            let currentPollSize = HS.size (pollVoters poll)
                nextPollState =
                  maybe poll
                    (\voterId -> fst $! addVoteToPoll ch voterId spamerId poll) mVoterId
                pollSizeChanged = currentPollSize /= HS.size (pollVoters nextPollState)
            pure $! Just (pollSizeChanged, nextPollState)

        -- at this point poll *must* be created
        forM_ mPoll $ \poll -> proceedWithPoll ch chatId spamerId mVoterId (Just orig) poll

-- | This message is definitely a spam! So let's:
--
-- 1. remove it.
-- 2. add them to list of spam messages.
-- 3. add spamer to blocklist.
updateBlocklistAndMessages
  :: (WithBotState, MonadIO m) => ChatId -> MessageInfo -> m ()
updateBlocklistAndMessages chatId MessageInfo{..} = do
  let BotState {..} = ?model
  void $ call $ deleteMessage (chatInfoId messageInfoChat) messageInfoId
  forM_ messageInfoText $ \txt' -> do
    let txt = MessageText txt'
    alterCache spamMessages txt (Just . succ . fromMaybe 1)
    case messageInfoFrom of
      Nothing -> pure () -- FIXME: there is a gap between spamers and messages
      Just user -> updateBlocklist chatId user (Just txt)

updateBlocklist
  :: (WithBotState, MonadIO m) => ChatId -> UserInfo -> Maybe MessageText -> m ()
updateBlocklist chatId user mMessage =
  let BotState{..} = ?model
      goBans Nothing = Just $! newBanState
        { bannedMessages = maybe HS.empty HS.singleton mMessage
        , bannedChats = HS.singleton chatId
        }
      goBans (Just hs@BanState{..}) =
        Just $! hs
          { bannedMessages = maybe bannedMessages (flip HS.insert bannedMessages) mMessage
          , bannedChats = HS.insert chatId bannedChats
          }
  in alterBlocklist blocklist user goBans 

hasUserAlreadyBannedElsewhere :: WithBotState => UserId -> BotM Bool
hasUserAlreadyBannedElsewhere userId =
  let BotState {..} = ?model in lookupBlocklist blocklist userId >>= pure . isJust

proceedWithPoll
  :: WithBotState
  => ChatState
  -> ChatId
  -> SpamerId
  -> Maybe VoterId
  -> Maybe MessageInfo
  -> (Bool, PollState)
  -> BotM ()
proceedWithPoll ch@ChatState{..} chatId spamerId voterId mOrig (pollChanged, poll@PollState{..}) = do
  let consensus = usersForConsensus chatSettings
      voters = HS.size pollVoters
      checkVoterId = (`HS.member` chatAdmins) . coerce @_ @UserId
      enoughToBan = fromIntegral consensus <= voters || maybe False checkVoterId voterId
  case (not pollChanged, enoughToBan) of
    -- Poll message has been originated, nothing to worry about
    (True, _) -> pure ()
    (False, False) ->
      updateBanPoll ch chatId spamerId voters consensus poll
    (_, True)  -> do
      forM_ mOrig $! updateBlocklistAndMessages chatId
      unlessDebug $! banSpamerInChat chatId pollSpamer
      closeBanPoll ch chatId spamerId
      void $ call $ deleteMessage chatId pollSpamMessageId
      void $ call $ deleteMessage chatId pollMessageId
      removeAllQuarantineMessages ch chatId spamerId
      selfDestructReply chatId ch (ReplyConsensus voters)

handleVoteBan
  :: WithBotState
  => ChatId
  -> ChatState
  -> VoterId
  -> MessageId -- it comes from callback, worth take a look at it
  -> VoteBanId
  -> BotM ()
handleVoteBan chatId ch@ChatState{..} voterId _messageId voteBanId = do
  liftIO $ log' @Text "handleVoteBan"
  let spamerId = voteBanIdToSpamerId voteBanId
      selfVote = coerce @SpamerId @UserId spamerId == coerce @VoterId @UserId voterId
  liftIO $ log' (selfVote, not isDebugEnabled)
  -- normally we do not want to vote for/against self
  -- but it is needed for debug only
  if selfVote && not isDebugEnabled
    then pure ()
    else case HM.lookup spamerId activePolls of
      -- Race? Poll's been closed but decision was too late? Don't care
      Nothing -> pure ()
      Just poll@PollState{pollMessageId, pollSpamer} -> case voteBanId of
        -- One independent voice is enough to close the poll
        VoteAgainstBan _ _ -> do
          closeBanPoll ch chatId spamerId
          void $ call $ deleteMessage chatId pollMessageId
          selfDestructReply chatId ch (ReplyUserRecovered pollSpamer)
        VoteForBan _ _ -> do
          let (newPoll, nextChatState) = addVoteToPoll ch voterId spamerId poll
              pollChanged = HS.size (pollVoters poll) /= HS.size (pollVoters newPoll)
          proceedWithPoll nextChatState chatId spamerId (Just voterId) Nothing (pollChanged, newPoll)

closeBanPoll :: WithBotState => ChatState -> ChatId -> SpamerId -> BotM ()
closeBanPoll st@ChatState{..} chatId spamerId = do
  let BotState {..} = ?model
  let nextChatState = st { activePolls = HM.delete spamerId activePolls }
  writeCache groups chatId nextChatState

allowUserInGroup :: WithBotState => ChatState -> ChatId -> UserId -> BotM ()
allowUserInGroup st@ChatState{..} chatId userId = do
  let BotState {..} = ?model
  let nextChatState = st { allowlist = HS.insert userId allowlist }
  writeCache groups chatId nextChatState

removeAllQuarantineMessages :: WithBotState => ChatState -> ChatId -> SpamerId -> BotM ()
removeAllQuarantineMessages ChatState{..} chatId spamerId = do
  let mQuarantine = HM.lookup (coerce @_ @UserId spamerId) quarantine
  forM_ mQuarantine \QuarantineState{..} -> 
    forM_ (Set.toList quarantineMessageId) (void . call . deleteMessage chatId )

createBanPoll
  :: WithBotState
  => ChatState
  -> ChatId
  -> SpamerId
  -> Maybe VoterId
  -> Voter
  -> Integer -- votes required for ban decision
  -> UserInfo -- spamer
  -> MessageId
  -> BotM (Maybe (Bool, PollState))
createBanPoll st chatId spamerId mVoterId voter consensus spamer messageId = do
  let BotState{..} = ?model
  let keyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = voteButtons chatId spamerId
        }
      replyMsg = (toReplyMessage (voteMessage voter (maybe 0 (const 1) mVoterId) consensus))
        { replyMessageReplyToMessageId = Just messageId
        , replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup keyboard
        }
      req = replyMessageToSendMessageRequest (SomeChatId chatId) replyMsg
  mResponse <- call $ sendMessage req
  case mResponse of
    Nothing -> pure Nothing
    Just Response{..} -> if not responseOk
      then pure Nothing
      else do
        let pollId = messageMessageId responseResult
            (poll, nextChatState) = startBanPoll st mVoterId spamerId spamer pollId messageId
        writeCache groups chatId nextChatState
        pure $ Just (False, poll)

updateBanPoll
  :: WithBotState
  => ChatState
  -> ChatId
  -> SpamerId
  -> Int
  -> Integer
  -> PollState
  -> BotM ()
updateBanPoll st@ChatState{..} chatId spamerId voters consensus poll@PollState{..} = do
  let BotState {..} = ?model
  let nextChatState = st { activePolls = HM.insert spamerId poll activePolls }
  writeCache groups chatId nextChatState

  let keyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = voteButtons chatId spamerId
        }
      editMsgTxt = voteMessage UserVoter voters consensus
      editMsg = (toEditMessage editMsgTxt)
        { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup keyboard
        }
      editId = EditChatMessageId (SomeChatId chatId) pollMessageId

  editMessage editId editMsg

data Voter = BotVoter | UserVoter
  deriving (Eq, Show)

voteMessage :: Voter -> Int -> Integer -> Text
voteMessage voter voters consensus = Text.concat
  [ textVoter
  , " decided that this is a spamer. Is it correct? Vote ("
  , s2t voters , "/", s2t consensus, ")"
  ]
  where
    textVoter = case voter of
      BotVoter -> "Bot"
      UserVoter -> "Someone"

voteButtons :: ChatId -> SpamerId -> [[InlineKeyboardButton]]
voteButtons chatId spamerId =
   [ makeButton <$> [ ("Yes", VoteForBan chatId spamerId), ("No", VoteAgainstBan chatId spamerId) ] ]
  where
    makeButton = uncurry actionButton

handleBotBanAction
  :: WithBotState => ChatId -> ChatState -> UserId -> ChatMember -> BotM ()
handleBotBanAction chatId ChatState{..} botUserId bannedMember = do
  let BotState {..} = ?model
  let botIsAdmin' = botUserId `HS.member` chatAdmins
      spamer = userToUserInfo $ chatMemberUser bannedMember
      go = Just . fromMaybe newBanState

  when botIsAdmin' $ do
    now <- liftIO getCurrentTime
    let evt = (chatEvent now chatId EventGroupSpam)
          { eventData = Just "by_other_admin_bot" }
    sendEvent evt

    alterBlocklist blocklist spamer go
