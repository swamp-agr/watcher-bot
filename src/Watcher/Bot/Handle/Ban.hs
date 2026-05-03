module Watcher.Bot.Handle.Ban where

import Control.Monad (forM_, void, when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Telegram.Bot.API
import Telegram.Bot.API.Names
import Telegram.Bot.Simple

import qualified Data.HashSet as HS
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector.Hashtables as HT

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
  forwardToOwnersMaybe Spam chatId (messageInfoId orig)
  voterIsAdmin <- liftIO (isJust <$> HT.lookup chatStateAdmins (coerce @_ @UserId voterId))
  if voterIsAdmin
    then handleBanByAdmin chatId ch orig
    else do
      let banReporter = case ((VoterId . userInfoId) <$> messageInfoFrom orig) == Just voterId of
            True -> BySpamer (coerce voterId)
            False -> ByUser voterId
      handleBanByRegularUser chatId ch banReporter orig
  -- remove bot message itself
  void $ call $ deleteMessage chatId messageId

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
  userIsAdmin <- liftIO (isJust <$> HT.lookup chatStateAdmins userId)
  let spamerId = adminBanIdToSpamerId adminBanId
  when userIsAdmin $ do
    case adminBanId of
      AdminAgainstBan _ _ -> do
        void $ call $ deleteMessage chatId messageId
        liftIO $ HT.delete chatStateAdminCalls spamerId
        writeCache groups chatId ch
        pure ()
      AdminForBan _ _ -> do
        void $ call $ deleteMessage chatId messageId
        (liftIO $ HT.lookup chatStateAdminCalls spamerId) >>= \case
          Nothing -> pure ()
          Just (spamer, orig) -> do
            updateBlocklistAndMessages chatId [] orig
            banSpamerInChat chatId spamer
            removeAllQuarantineMessages ch chatId spamerId
            liftIO do
              HT.delete chatStateAdminCalls spamerId
              HT.delete chatStateQuarantine (coerce @_ @UserId spamerId)
            writeCache groups chatId ch
            selfDestructReply chatId ch (ReplyUserAlreadyBanned spamer)

banSpamerInChat :: WithBotState => ChatId -> UserInfo -> BotM ()
banSpamerInChat chatId spamer = do
  now <- liftIO getCurrentTime
  sendEvent ((chatEvent now chatId EventGroupBan) { eventUserId = Just $ userInfoId spamer })

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
  updateBlocklistAndMessages chatId [] orig

  -- according to telegram it should be empty only in channels
  forM_ mSpamerUser \spamer -> do
    -- remove poll if exists
    let spamerId = SpamerId $ userInfoId spamer
    mPoll <- liftIO $ HT.lookup chatStateActivePolls spamerId
    forM_ (pollMessageId <$> mPoll) (void . call . deleteMessage chatId)
    closeBanPoll ch chatId spamerId

    -- remove all messages tracked in quarantine
    removeAllQuarantineMessages ch chatId spamerId
    
    banSpamerInChat chatId spamer

handleBanByRegularUser
  :: WithBotState => ChatId -> ChatState -> BanRequestedBy -> MessageInfo -> BotM ()
handleBanByRegularUser chatId ch@ChatState{..} banReporter orig = do
  now <- liftIO getCurrentTime
  let evtLog = case banReporter of
        ByBot _ -> "by_bot"
        ByAdmin _ -> "by_admin"
        BySpamer _ -> "by_spamer"
        ByUser _ -> "by_user"
      evt = (chatEvent now chatId EventGroupSpam)
        { eventData = Just evtLog }
  sendEvent evt

  let GroupSettings{spamCommandAction, usersForConsensus} = chatStateSettings
      mSpamerUser = messageInfoFrom orig
  forM_ mSpamerUser $ \spamer -> case spamCommandAction of
    SCPoll ->
      handleBanViaConsensusPoll chatId ch banReporter spamer orig usersForConsensus
    SCAdminsCall -> handleBanViaAdminsCall chatId ch spamer orig

handleBanViaAdminsCall
  :: WithBotState => ChatId -> ChatState -> UserInfo -> MessageInfo -> BotM ()
handleBanViaAdminsCall chatId ch@ChatState{..} spamer orig = do
  let BotState {..} = ?model
      spamerId = SpamerId $! userInfoId spamer
  liftIO $ HT.insert chatStateAdminCalls spamerId (spamer, orig)
  writeCache groups chatId ch

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
  -> BanRequestedBy
  -> UserInfo
  -> MessageInfo
  -> Integer
  -> BotM ()
handleBanViaConsensusPoll chatId ch@ChatState{..} banReporter spamer orig consensus = do
  let spamerId = SpamerId $! userInfoId spamer
  userAlreadyBanned <- hasUserAlreadyBannedElsewhere (userInfoId spamer)
  if userAlreadyBanned
    then do
      updateBlocklistAndMessages chatId [] orig
      banSpamerInChat chatId spamer
      removeAllQuarantineMessages ch chatId spamerId
      selfDestructReply chatId ch (ReplyUserAlreadyBanned spamer)
    else do
      adminReported <- liftIO (isJust <$> HT.lookup chatStateAdmins (coerce spamerId))
      let selfReport = case banReporter of
            BySpamer _ -> True
            _ -> False
      mPollState <- liftIO $ HT.lookup chatStateActivePolls spamerId

      unless (adminReported || selfReport) $ do
        mPoll <- case mPollState of
          Nothing -> createBanPoll ch chatId spamerId banReporter consensus spamer
            $! messageInfoId orig
          Just poll -> do
            let currentPollSize = HS.size (pollVoters poll)
            nextPollState <- case banReporter of
              ByBot _ -> pure poll
              BySpamer _ -> pure poll
              ByUser userId -> fst <$> addVoteToPoll ch (userId) spamerId poll
              ByAdmin userId -> fst <$> addVoteToPoll ch (VoterId userId) spamerId poll
            let pollSizeChanged = currentPollSize /= HS.size (pollVoters nextPollState)
            pure $! Just (pollSizeChanged, nextPollState)

        -- at this point poll *must* be created
        forM_ mPoll $ \poll -> proceedWithPoll ch chatId spamerId (toVoterId banReporter) (Just orig) poll

-- | This message is definitely a spam! So let's:
--
-- 1. remove it.
-- 2. add them to list of spam messages.
-- 3. add spamer to blocklist.
updateBlocklistAndMessages
  :: (WithBotState, MonadIO m) => ChatId -> [MessageText] -> MessageInfo -> m ()
updateBlocklistAndMessages chatId extraMessages MessageInfo{..} = do
  let BotState {..} = ?model
  void $ call $ deleteMessage (chatInfoId messageInfoChat) messageInfoId
  forM_ messageInfoText $ \txt' -> do
    let txt = MessageText txt'
    alterCache spamMessages txt (Just . succ . fromMaybe 1)
    case messageInfoFrom of
      Nothing -> pure () -- FIXME: there is a gap between spamers and messages
      Just user -> updateBlocklist chatId user (txt : extraMessages)

updateBlocklist
  :: (WithBotState, MonadIO m) => ChatId -> UserInfo -> [MessageText] -> m ()
updateBlocklist chatId user messages = do
  nbs <- liftIO newBanState
  let BotState{..} = ?model
  messageSet <- liftIO $ toHSet (HS.fromList messages)
  singleChat <- liftIO $ HT.fromList [(chatId, ())]
  let goBans Nothing = pure $ Just $! nbs
        { banStateMessages = messageSet
        , banStateChats = singleChat
        }
      goBans (Just hs) = do
        nextMessages <- liftIO $ HT.union messageSet (banStateMessages hs)
        HT.insert (banStateChats hs) chatId ()
        pure $! Just $! hs { banStateMessages = nextMessages }
  alterBlocklistM blocklist user goBans 

hasUserAlreadyBannedElsewhere :: WithBotState => UserId -> BotM Bool
hasUserAlreadyBannedElsewhere userId =
  let BotState {..} = ?model in lookupBlocklist blocklist userId >>= pure . isJust

proceedWithPoll
  :: WithBotState
  => ChatState
  -> ChatId
  -> SpamerId
  -> VoterId
  -> Maybe MessageInfo
  -> (Bool, PollState)
  -> BotM ()
proceedWithPoll ch@ChatState{..} chatId spamerId vid mOrig (pollChanged, poll@PollState{..}) = do
  voterIsAdmin <- liftIO (isJust <$> HT.lookup chatStateAdmins (coerce @_ @UserId vid))
  let consensus = usersForConsensus chatStateSettings
      voters = HS.size pollVoters
      enoughToBan = fromIntegral consensus <= voters || voterIsAdmin
  case (not pollChanged, enoughToBan) of
    -- Poll message has been originated, nothing to worry about
    (True, False) -> pure ()
    (False, False) ->
      updateBanPoll ch chatId spamerId voters consensus poll
    (_, True)  -> do
      -- remove spam message first thing
      void $ call $ deleteMessage chatId pollSpamMessageId
      forM_ mOrig $! updateBlocklistAndMessages chatId []
      unlessDebug $! banSpamerInChat chatId pollSpamer
      closeBanPoll ch chatId spamerId
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
    else (liftIO $ HT.lookup chatStateActivePolls spamerId) >>= \case
      -- Race? Poll's been closed but decision was too late? Don't care
      Nothing -> pure ()
      Just poll@PollState{pollMessageId, pollSpamer} -> case voteBanId of
        -- One independent voice is enough to close the poll
        VoteAgainstBan _ _ -> do
          closeBanPoll ch chatId spamerId
          void $ call $ deleteMessage chatId pollMessageId
          selfDestructReply chatId ch (ReplyUserRecovered pollSpamer)
        VoteForBan _ _ -> do
          (newPoll, nextChatState) <- addVoteToPoll ch voterId spamerId poll
          let pollChanged = HS.size (pollVoters poll) /= HS.size (pollVoters newPoll)
          proceedWithPoll nextChatState chatId spamerId voterId Nothing (pollChanged, newPoll)

closeBanPoll :: WithBotState => ChatState -> ChatId -> SpamerId -> BotM ()
closeBanPoll st@ChatState{..} chatId spamerId = do
  let BotState {..} = ?model
  liftIO $ HT.delete chatStateActivePolls spamerId
  writeCache groups chatId st

allowUserInGroup :: WithBotState => ChatState -> ChatId -> UserId -> BotM ()
allowUserInGroup st@ChatState{..} chatId userId = do
  let BotState {..} = ?model
  liftIO $ HT.insert chatStateAllowlist userId ()
  writeCache groups chatId st

removeAllQuarantineMessages :: WithBotState => ChatState -> ChatId -> SpamerId -> BotM ()
removeAllQuarantineMessages ChatState{..} chatId spamerId = do
  mQuarantine <- liftIO $ HT.lookup chatStateQuarantine (coerce @_ @UserId spamerId)
  forM_ mQuarantine \QuarantineState{..} -> 
    forM_ (Set.toList quarantineMessageId) (void . call . deleteMessage chatId )

createBanPoll
  :: WithBotState
  => ChatState
  -> ChatId
  -> SpamerId
  -> BanRequestedBy
  -> Integer -- votes required for ban decision
  -> UserInfo -- spamer
  -> MessageId
  -> BotM (Maybe (Bool, PollState))
createBanPoll st chatId spamerId banReporter consensus spamer messageId = do
  let BotState{..} = ?model
  let keyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = voteButtons chatId spamerId
        }
      isBot = case banReporter of
        ByBot _ -> True
        _       -> False
      replyMsg = (toReplyMessage (voteMessage isBot 1 consensus))
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
        (poll, nextChatState) <- startBanPoll st banReporter spamerId spamer pollId messageId
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
  liftIO $ HT.insert chatStateActivePolls spamerId poll
  writeCache groups chatId st

  let keyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = voteButtons chatId spamerId
        }
      editMsgTxt = voteMessage False voters consensus
      editMsg = (toEditMessage editMsgTxt)
        { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup keyboard
        }
      editId = EditChatMessageId (SomeChatId chatId) pollMessageId

  editMessage editId editMsg

voteMessage :: Bool -> Int -> Integer -> Text
voteMessage isBot voters consensus = Text.concat
  [ textVoter
  , " decided that this is a spamer. Is it correct? Vote ("
  , s2t voters , "/", s2t consensus, ")"
  ]
  where
    textVoter = case isBot of
      True   -> "Bot"
      False  -> "Someone"

voteButtons :: ChatId -> SpamerId -> [[InlineKeyboardButton]]
voteButtons chatId spamerId =
   [ makeButton <$> [ ("Yes", VoteForBan chatId spamerId), ("No", VoteAgainstBan chatId spamerId) ] ]
  where
    makeButton = uncurry actionButton

handleBotBanAction
  :: WithBotState => ChatId -> ChatState -> UserId -> ChatMember -> BotM ()
handleBotBanAction chatId ChatState{..} botUserId bannedMember = do
  nbs <- liftIO newBanState
  let BotState {..} = ?model
  botIsAdmin' <- liftIO (isJust <$> HT.lookup chatStateAdmins botUserId)
  let spamer = userToUserInfo $ chatMemberUser bannedMember
      go = Just . fromMaybe nbs

  when botIsAdmin' $ do
    now <- liftIO getCurrentTime
    let evt = (chatEvent now chatId EventGroupSpam)
          { eventData = Just "by_other_admin_bot" }
    sendEvent evt

    alterBlocklist blocklist spamer go
