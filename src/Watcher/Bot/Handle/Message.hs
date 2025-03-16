module Watcher.Bot.Handle.Message where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Monad (forM, forM_, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Char (ord)
import Data.Foldable (asum)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime)
import Dhall (Natural)
import Telegram.Bot.API
import Telegram.Bot.API.Names
import Telegram.Bot.Simple

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Watcher.Bot.Analytics
import Watcher.Bot.Cache
import Watcher.Bot.Handle.Ban
import Watcher.Bot.Handle.Contact
import Watcher.Bot.Reply
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.State.Chat
import Watcher.Bot.Types
import Watcher.Bot.Utils

handleAnalyseMessage :: WithBotState => ChatId -> UserId -> Message -> BotM ()
handleAnalyseMessage chatId userId message = do
  let BotState {..} = ?model
  mChatState <- lookupCache groups chatId
  forM_ mChatState $ \ch@ChatState{..} -> case chatSetup of
    SetupNone -> pure ()
    SetupInProgress {} -> pure ()
    SetupCompleted {} -> analyseMessage chatId ch userId message

handleTuning :: WithBotState => Update -> BotM ()
handleTuning Update{..} = do
  let BotState {..} = ?model
      mMsg = asum [ updateMessage, updateEditedMessage ]
  forM_ mMsg $ \origMsg -> do
    void $ call $ deleteMessage (chatId $ messageChat origMsg) (messageMessageId origMsg)
    forM_ (messageReplyToMessage origMsg) $ \msg ->
      forM_ (messageForwardOrigin msg) $ \origin -> case origin of
        MessageOriginUser{messageOriginUserSenderUser} -> do
          let userId' = userId messageOriginUserSenderUser
              userChatId = coerce @_ @ChatId userId'
          mResponse <- call $ getChat (SomeChatId userChatId)
          liftIO $ log' ("ChatFullInfo" :: Text, mResponse)
          let mChat = responseResult <$> mResponse
              qs = emptyQuarantineState { quarantineUserChatInfo = toChatInfo <$> mChat }
              ch = (newChatState botSettings) { quarantine = HM.singleton userId' qs }
              decision = decideAboutMessage ch messageOriginUserSenderUser msg
          replyTuning (messageMessageId msg) (messageMessageThreadId msg) decision
        _ -> pure ()

analyseMessage :: WithBotState => ChatId -> ChatState -> UserId -> Message -> BotM ()
analyseMessage chatId ch userId message = do
  forM_ (messageNewChatMembers message) $ addToQuarantineOrBan chatId ch message

  let messageInfo = messageToMessageInfo message
      fullBan banType extraMessages = forM_ (messageFrom message) \spamerUser -> do
        let spamer = userToUserInfo spamerUser
        updateBlocklistAndMessages chatId extraMessages messageInfo
        banSpamerInChat chatId spamer
        removeAllQuarantineMessages ch chatId (SpamerId userId)
        selfDestructReply chatId ch (banType spamer)
  userAlreadyBanned <- hasUserAlreadyBannedElsewhere userId
  if userAlreadyBanned
    -- if user has banned globally but was unbanned locally, bot will allow such a user
    then do
      unless (HS.member userId (allowlist ch)) do
        fullBan ReplyUserAlreadyBanned []
    else callCasCheck userId >>= \case
    Just messages -> do
      let texts = MessageText <$> messages
      fullBan ReplyUserCASBanned texts
    Nothing -> do
      knownSpamMessage <- isKnownSpamMessage messageInfo
      if knownSpamMessage
        then handleBanByRegularUser chatId ch Nothing messageInfo
        else case userIsInChatQuarantine ch userId of
          Nothing -> pure ()
          Just inQuarantine -> forM_ (messageFrom message) $ \user -> do
            now <- liftIO getCurrentTime
            case decideAboutMessage ch user message of
              RegularMessage _ ->
                incrementQuarantineCounter chatId ch userId inQuarantine message
              ProbablySpamMessage _ ->
                sendEvent (chatEvent now chatId EventGroupRecogniseProbablySpam)
              MostLikelySpamMessage _ -> do
                sendEvent (chatEvent now chatId EventGroupRecogniseMostLikelySpam)
                forwardToOwnersMaybe Spam chatId (messageInfoId messageInfo)
                handleBanByRegularUser chatId ch Nothing messageInfo

incrementQuarantineCounter
  :: WithBotState => ChatId -> ChatState -> UserId -> Int -> Message -> BotM ()
incrementQuarantineCounter chatId ch@ChatState{..} userId inQuarantine Message{..} = do
  let BotState {..} = ?model
  forM_ messageText $ \txt -> do
    let GroupSettings{..} = chatSettings
        enoughToRelease = inQuarantine + 1 == fromIntegral messagesInQuarantine
        hash = hexSha256 txt
    if enoughToRelease
      then endQuarantineForUser chatId userId
      else do
        let go Nothing = Just $! emptyQuarantineState
              { quarantineMessageHash = Set.singleton hash
              , quarantineMessageId = Set.singleton messageMessageId
              }
            go (Just qs@QuarantineState{..}) = Just $! qs
              { quarantineMessageHash = Set.insert hash quarantineMessageHash
              , quarantineMessageId = Set.insert messageMessageId quarantineMessageId
              }
            nextState = ch { quarantine = HM.alter go userId quarantine }
        writeCache groups chatId nextState

userIsInChatQuarantine :: ChatState -> UserId -> Maybe Int
userIsInChatQuarantine ChatState{..} userId =
  HM.lookup userId quarantine >>= pure . Set.size . quarantineMessageHash

endQuarantineForUser :: (WithBotState, MonadIO m) => ChatId -> UserId -> m ()
endQuarantineForUser chatId userId = do
  let BotState {..} = ?model
      go Nothing = Nothing
      go (Just ch@ChatState{..}) = Just $! ch { quarantine = HM.delete userId quarantine }
  alterCache groups chatId go

addToQuarantineOrBan :: WithBotState => ChatId -> ChatState -> Message -> [User] -> BotM ()
addToQuarantineOrBan chatId ch@ChatState{..} message newcomers = do
  let BotState {..} = ?model
  newcomersWithChats <- forM newcomers $ \newcomer -> do
    let uid = userId newcomer
        userInfo = userToUserInfo newcomer

        fullBanAction bs@BanState{..} banType messages = do
          let messageSet = HS.fromList (MessageText <$> messages)
              nextBanState = bs
                { bannedChats = HS.insert chatId bannedChats
                , bannedMessages = HS.union messageSet bannedMessages
                }
              spamer = userToUserInfo newcomer
              spamerId = SpamerId $! userInfoId spamer
          alterBlocklist blocklist userInfo $! const $! Just nextBanState

          banSpamerInChat chatId spamer
          removeAllQuarantineMessages ch chatId spamerId
          selfDestructReply chatId ch (banType spamer)

    lookupBlocklist blocklist uid >>= \case
      Just bs -> do
        fullBanAction bs ReplyUserAlreadyBanned []
        pure Nothing

      Nothing -> callCasCheck uid >>= \case

        Just messages -> do
          fullBanAction newBanState ReplyUserCASBanned messages
          pure Nothing

        Nothing -> do
          now <- liftIO getCurrentTime
          userIsSpammer <- case decideAboutMessage ch newcomer message of
            RegularMessage _ -> pure False
            ProbablySpamMessage _ -> do
              sendEvent (chatEvent now chatId EventGroupRecogniseProbablySpam)
              forwardToOwnersMaybe Spam chatId (messageMessageId message)
              handleBanByRegularUser chatId ch Nothing (messageToMessageInfo message)
              pure True
            MostLikelySpamMessage _ -> do
              sendEvent (chatEvent now chatId EventGroupRecogniseMostLikelySpam)
              forwardToOwnersMaybe Spam chatId (messageMessageId message)
              handleBanByRegularUser chatId ch Nothing (messageToMessageInfo message)
              pure True
          case userIsSpammer of
            True -> pure Nothing
            False -> do
              let userChatId = SomeChatId $ coerce @_ @ChatId uid
                  evt = UserChatMemberCheckEvent $! UserChatMemberCheck
                    { userChatMemberCheckChatId = chatId
                    , userChatMemberCheckUserInfo = userInfo
                    , userChatMemberCheckTime = addUTCTime (5 * 60) now
                    }
              mResponse <- call $ getChat userChatId
              liftIO $ atomically $! modifyTVar' eventSet $! Set.insert evt
              pure $ Just ((toChatInfo . responseResult) <$> mResponse, newcomer)

  let toQuarantineEntry (mChat, User{..}) =
        (userId, emptyQuarantineState { quarantineUserChatInfo = mChat })
      newUserMap = HM.fromList (toQuarantineEntry <$> catMaybes newcomersWithChats)
      go prev _new = prev -- current strategy: leave previous state, do not flush it
      nextState = ch { quarantine = HM.unionWith go quarantine newUserMap }

  writeCache groups chatId nextState


data UserMessageScore = UserMessageScore
  { userMessageScoreNoUsername :: Int
  , userMessageScoreEmojiInName :: Int
  , userMessageScoreIsPremium :: Int
  , userMessageScoreAdult :: Int
  , userMessageScoreKnownName :: Int
  , userMessageScoreRichMarkup :: Int
  , userMessageScoreWords :: Map Text Int
  , userMessageScoreCopyPaste :: Int
  , userMessageScoreUsernameWords :: Map Text Int
  }
  deriving (Eq, Show)

userMessageScoreToText :: UserMessageScore -> Text
userMessageScoreToText ums@UserMessageScore{..} = Text.unlines
  [ wrap "No username" userMessageScoreNoUsername
  , wrap "Name contains emoji" userMessageScoreEmojiInName
  , wrap "Premium" userMessageScoreIsPremium
  , wrap "Adult score" userMessageScoreAdult
  , wrap "Known spamer user name" userMessageScoreKnownName
  , wrap "Rich markup in message" userMessageScoreRichMarkup
  , wrapMap "Words" (Map.toList userMessageScoreWords)
  , wrap "Copy Paste" userMessageScoreCopyPaste
  , wrapMap "Username words" (Map.toList userMessageScoreUsernameWords)
  , ""
  , wrap "Total score" (getTotalScore ums)
  ]
  where
    b x = Text.concat [ "<b>", x, "</b>" ]
    wrap label n = Text.concat [ b label, ": ", s2t n ]
    wrapMap label wordMap = Text.concat
      [ b label, ":\n", Text.unlines (wrapWordScore <$> wordMap) ]
    wrapWordScore (word, score) = Text.concat
      [ " - ", b word, ": ", s2t score ]

getTotalScore :: UserMessageScore -> Int
getTotalScore UserMessageScore{..} = sum $
  [ userMessageScoreNoUsername
  , userMessageScoreEmojiInName
  , userMessageScoreIsPremium
  , userMessageScoreAdult
  , userMessageScoreKnownName
  , userMessageScoreRichMarkup
  , userMessageScoreCopyPaste
  ] <> Map.elems userMessageScoreWords <> Map.elems userMessageScoreUsernameWords

data MessageDecision
  = RegularMessage UserMessageScore
  | ProbablySpamMessage UserMessageScore
  | MostLikelySpamMessage UserMessageScore
  deriving (Eq, Show)

messageDecisionToText :: MessageDecision -> Text
messageDecisionToText = \case
  RegularMessage _ -> "Regular message"
  ProbablySpamMessage _ -> "Probably spam"
  MostLikelySpamMessage _ -> "Most likely it is a spam"

messageDecisionToScore :: MessageDecision -> UserMessageScore
messageDecisionToScore = \case
  RegularMessage score -> score
  ProbablySpamMessage score -> score
  MostLikelySpamMessage score -> score

isKnownSpamMessage :: WithBotState => MessageInfo -> BotM Bool
isKnownSpamMessage msg =
  let BotState {..} = ?model
  in maybe (pure False)
     (\txt -> lookupCache spamMessages (MessageText txt) >>= pure . isJust)
     (messageInfoText msg)

getUserScore
  :: (User -> Bool)
  -> (ScoreSettings -> Natural)
  -> ScoreSettings
  -> User
  -> Int
getUserScore userPredicate scoreField scores user = if userPredicate user
  then (fromIntegral . scoreField) scores
  else 0

hasNoUsername :: ScoreSettings -> User -> Int
hasNoUsername = getUserScore go scoreUserHasNoUsername
  where
    go u = (isNothing . userUsername) u || (Text.null . getUserName) u

doesNameContainEmoji :: ChatState -> ScoreSettings -> User -> Int
doesNameContainEmoji ChatState{..} = getUserScore go scoreUserNameContainsEmoji
  where
    go :: User -> Bool
    go u =
      let mChat = quarantineUserChatInfo =<< HM.lookup (userId u) quarantine
          txt = getUserName u

          chatContainsAnyEmoji ChatInfo{..} =
            isJust chatInfoEmojiStatusCustomEmojiId
            || isJust chatInfoBackgroundCustomEmojiId
      in containAnyEmoji txt || (maybe False chatContainsAnyEmoji mChat)

-- FIXME: handle composite emoji too
containAnyEmoji :: Text -> Bool
containAnyEmoji = Text.any isSingleEmoji

isSingleEmoji :: Char -> Bool
isSingleEmoji char =
  let n = ord char
  in  126976 {- 0x1f000 -} <= n && n <= 129504 {- 0x1F9E0 -}


isKnownSpamerName :: ScoreSettings -> User -> Int
isKnownSpamerName ScoreSettings{scoreUserKnownSpamerNames} u =
  fromIntegral $! fromMaybe 0 $!
    (Map.lookup (getUserName u) scoreUserKnownSpamerNames
     <|> Map.lookup (Text.toLower $ userFirstName u) scoreUserKnownSpamerNames)

isPremiumScore :: ScoreSettings -> User -> Int
isPremiumScore = getUserScore ((== Just True) . userIsPremium) scoreUserHasPremium

getUserAdultScore :: WithBotState => ChatState -> ScoreSettings -> User -> Int
getUserAdultScore ChatState{..} ScoreSettings{..} u =
  let BotState {..} = ?model
      go n c = n + if HS.member c adultEmoji then scoreUserAdultScore else 0
      computeScore txt = fromIntegral $ Text.foldl' go 0 txt
      getChatScore ChatInfo{..} = computeScore chatInfoName
        + maybe 0 computeScore chatInfoBio
      
      userScore = computeScore (getUserName u)
      mChat = quarantineUserChatInfo =<< HM.lookup (userId u) quarantine
      chatScore = maybe 0 getChatScore mChat
  in userScore + chatScore

messageContainsRichMarkup :: ScoreSettings -> Message -> Int
messageContainsRichMarkup ScoreSettings{..}
  = (* (fromIntegral scoreMessageContainsRichMarkup))
  . length
  . filter ((== MessageEntityCustomEmoji) . messageEntityType)
  . fromMaybe [] . messageEntities

messageWordsScore :: ScoreSettings -> Message -> Map Text Int
messageWordsScore score = textWordsScore score . fromMaybe "" . messageText

-- FIXME: use duckling
textWordsScore :: ScoreSettings -> Text -> Map Text Int
textWordsScore ScoreSettings{scoreMessageWordsScore} txt =
  let go wordMap word = ((word, ) . fromIntegral) <$> Map.lookup word wordMap
  
      getTotalWordsScore =
        Map.fromListWith (+)
        . catMaybes
        . fmap (go scoreMessageWordsScore . Text.toLower)
        . Text.words
  in getTotalWordsScore $! txt

messageUsernameWordsScore :: ScoreSettings -> User -> Map Text Int
messageUsernameWordsScore score = textWordsScore score . getUserName

messageCopyPasteScore :: ScoreSettings -> ChatState -> UserId -> Message -> Int
messageCopyPasteScore ScoreSettings{..} ChatState{..} userId Message{..} = 
  let messages = fromMaybe Set.empty
        $! fmap quarantineMessageHash
        $! HM.lookup userId quarantine
  in if Set.null messages
    then 0
    else let messageExist = fromMaybe False
               (flip Set.member messages . hexSha256 <$> messageText)
         in if messageExist
            then fromIntegral scoreCopyPaste
            else 0

decideAboutMessage
  :: WithBotState => ChatState -> User -> Message -> MessageDecision
decideAboutMessage ch user@User{userId} msg =
  let BotState {..} = ?model
      Settings{..} = botSettings
      
      userMessageScore = UserMessageScore
        { userMessageScoreNoUsername = hasNoUsername scores user
        , userMessageScoreEmojiInName = doesNameContainEmoji ch scores user
        , userMessageScoreIsPremium = isPremiumScore scores user
        , userMessageScoreAdult = getUserAdultScore ch scores user
        , userMessageScoreKnownName = isKnownSpamerName scores user
        , userMessageScoreRichMarkup = messageContainsRichMarkup scores msg
        , userMessageScoreWords = messageWordsScore scores msg
        , userMessageScoreCopyPaste = messageCopyPasteScore scores ch userId msg
        , userMessageScoreUsernameWords = messageUsernameWordsScore scores user
        }
      totalScore = getTotalScore userMessageScore

      majorThreshold = fromIntegral @_ @Int $ scoreMajorThreshold scores
      criticalThreshold = fromIntegral @_ @Int $ scoreCriticalThreshold scores

      mkDecision = if totalScore < majorThreshold then RegularMessage else
        if majorThreshold <= totalScore && totalScore < criticalThreshold
          then ProbablySpamMessage
          else MostLikelySpamMessage
  
  in mkDecision userMessageScore

renderDecision :: MessageDecision -> Text
renderDecision = \case
  RegularMessage score -> Text.unlines
    [ "<b>Regular message</b>\n", userMessageScoreToText score ]
  ProbablySpamMessage score -> Text.unlines
    [ "<b>Proably spam</b>\n", userMessageScoreToText score ]
  MostLikelySpamMessage score -> Text.unlines
    [ "<b>Most likely it is a spam</b>\n", userMessageScoreToText score ]


replyTuning :: MessageId -> Maybe MessageThreadId -> MessageDecision -> BotM ()
replyTuning messageId mMessageThreadId mDecision = do
  let decisionText = renderDecision mDecision
      replyMsg = (toReplyMessage decisionText)
        { replyMessageParseMode = Just HTML
        , replyMessageReplyToMessageId = Just messageId
        , replyMessageMessageThreadId = mMessageThreadId
        }
  reply replyMsg
