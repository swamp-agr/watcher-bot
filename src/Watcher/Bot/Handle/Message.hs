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
import qualified Data.Vector.Hashtables as HT

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
  forM_ mChatState $ \ch@ChatState{..} -> case chatStateSetup of
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
          ch <- liftIO $ newChatState botSettings
          liftIO $ HT.insert (chatStateQuarantine ch) userId' qs
          decision <- decideAboutMessage ch messageOriginUserSenderUser msg
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
      userIsAllowed <- liftIO (isJust <$> HT.lookup (chatStateAllowlist ch) userId)
      unless userIsAllowed do
        fullBan ReplyUserAlreadyBanned []
    else callCasCheck userId >>= \case
    Just messages -> do
      let texts = MessageText <$> messages
      fullBan ReplyUserCASBanned texts
    Nothing -> do
      knownSpamMessage <- isKnownSpamMessage messageInfo
      if knownSpamMessage
        then handleBanByRegularUser chatId ch Nothing messageInfo
        else userIsInChatQuarantine ch userId >>= \case
          Nothing -> pure ()
          Just inQuarantine -> forM_ (messageFrom message) $ \user -> do
            now <- liftIO getCurrentTime
            (liftIO $ decideAboutMessage ch user message) >>= \case
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
    let GroupSettings{..} = chatStateSettings
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
        liftIO $ HT.alter chatStateQuarantine go userId
        writeCache groups chatId ch

userIsInChatQuarantine :: MonadIO m => ChatState -> UserId -> m (Maybe Int)
userIsInChatQuarantine ChatState{..} userId = do
  mQuarantine <- liftIO $ HT.lookup chatStateQuarantine userId
  forM mQuarantine (pure . Set.size . quarantineMessageHash)

endQuarantineForUser :: (WithBotState, MonadIO m) => ChatId -> UserId -> m ()
endQuarantineForUser chatId userId = do
  let BotState {..} = ?model
  mChatState <- lookupCache groups chatId
  forM_ mChatState \ch@ChatState{..} -> do
    liftIO $ HT.delete chatStateQuarantine userId
    writeCache groups chatId ch

addToQuarantineOrBan :: WithBotState => ChatId -> ChatState -> Message -> [User] -> BotM ()
addToQuarantineOrBan chatId ch@ChatState{..} message newcomers = do
  let BotState {..} = ?model
  newcomersWithChats <- forM newcomers $ \newcomer -> do
    let uid = userId newcomer
        userInfo = userToUserInfo newcomer

        fullBanAction bs@BanState{..} banType messages = do
          messageSet <- liftIO $ toHSet $ HS.fromList (MessageText <$> messages)
          let goBans _ = liftIO do
                HT.insert banStateChats chatId ()
                nextMessages <- HT.union banStateMessages messageSet
                pure $! Just $! bs { banStateMessages = nextMessages }

              spamer = userToUserInfo newcomer
              spamerId = SpamerId $! userInfoId spamer

          liftIO $ alterBlocklistM blocklist userInfo $! goBans

          banSpamerInChat chatId spamer
          removeAllQuarantineMessages ch chatId spamerId
          selfDestructReply chatId ch (banType spamer)

    lookupBlocklist blocklist uid >>= \case
      Just bs -> do
        fullBanAction bs ReplyUserAlreadyBanned []
        pure Nothing

      Nothing -> callCasCheck uid >>= \case

        Just messages -> do
          nbs <- liftIO newBanState
          fullBanAction nbs ReplyUserCASBanned messages
          pure Nothing

        Nothing -> do
          now <- liftIO getCurrentTime
          userIsSpammer <- (liftIO $ decideAboutMessage ch newcomer message) >>= \case
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
      go prev _new = prev -- current strategy: leave previous state, do not flush it
  newUserMap <- liftIO $ HT.fromList (toQuarantineEntry <$> catMaybes newcomersWithChats)
  nextQuarantine <- liftIO $ HT.unionWith go chatStateQuarantine newUserMap
  let nextState = ch { chatStateQuarantine = nextQuarantine }

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
  , userMessageScoreQuotes :: Int
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
  , userMessageScoreQuotes
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

doesNameContainEmoji
  :: MonadIO m => ChatState -> ScoreSettings -> User -> m Int
doesNameContainEmoji ChatState{..} scores u = do
  mQuarantine <- liftIO $ HT.lookup chatStateQuarantine (userId u)
  let mChat = quarantineUserChatInfo =<< mQuarantine
      go :: User -> Bool
      go u =
        let txt = getUserName u

            chatContainsAnyEmoji ChatInfo{..} =
              isJust chatInfoEmojiStatusCustomEmojiId
              || isJust chatInfoBackgroundCustomEmojiId
        in containAnyEmoji txt || (maybe False chatContainsAnyEmoji mChat)
  pure $! getUserScore go scoreUserNameContainsEmoji scores u

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

getUserAdultScore
  :: (MonadIO m, WithBotState) => ChatState -> ScoreSettings -> User -> m Int
getUserAdultScore ChatState{..} ScoreSettings{..} u = do
  let BotState {..} = ?model
  mQuarantine <- liftIO $ HT.lookup chatStateQuarantine (userId u)

  let mChat = quarantineUserChatInfo =<< mQuarantine
      go c = do
        isAdultEmoji <- liftIO (isJust <$> HT.lookup adultEmoji c)
        pure $! if isAdultEmoji then scoreUserAdultScore else 0
      computeScore txt = do
        scores <- sequenceA (go <$> Text.unpack txt)
        pure $ fromIntegral $ sum scores
      getChatScore ChatInfo{..} = do
        nameScore <- computeScore chatInfoName
        bioScore <- maybe (pure 0) computeScore chatInfoBio
        pure $! nameScore + bioScore
      
  userScore <- computeScore (getUserName u)
  chatScore <- maybe (pure 0) getChatScore mChat
  pure $! userScore + chatScore

messageContainsRichMarkup :: ScoreSettings -> Message -> Int
messageContainsRichMarkup ScoreSettings{..}
  = (* (fromIntegral scoreMessageContainsRichMarkup))
  . length
  . filter ((== MessageEntityCustomEmoji) . messageEntityType)
  . fromMaybe [] . messageEntities

messageWordsScore :: ScoreSettings -> Message -> Map Text Int
messageWordsScore score = textWordsScore score . fromMaybe "" . messageText

messageQuoteScore :: ScoreSettings -> Message -> Int
messageQuoteScore ScoreSettings{..}
  = (* (fromIntegral scoreMessageQuote))
  . length
  . filter ((`elem` [MessageEntityBlockquote, MessageEntityExpandableBlockquote]) . messageEntityType)
  . fromMaybe [] . messageEntities

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

messageCopyPasteScore
  :: MonadIO m => ScoreSettings -> ChatState -> UserId -> Message -> m Int
messageCopyPasteScore ScoreSettings{..} ChatState{..} userId Message{..} = do
  mQuarantine <- liftIO $ HT.lookup chatStateQuarantine userId
  let messages = fromMaybe Set.empty
        $! fmap quarantineMessageHash
        $! mQuarantine
  pure $ if Set.null messages
    then 0
    else let messageExist = fromMaybe False
               (flip Set.member messages . hexSha256 <$> messageText)
         in if messageExist
            then fromIntegral scoreCopyPaste
            else 0

decideAboutMessage
  :: (MonadIO m, WithBotState) => ChatState -> User -> Message -> m MessageDecision
decideAboutMessage ch user@User{userId} msg = do
  let BotState {..} = ?model
      Settings{..} = botSettings

  userMessageScoreCopyPaste <- messageCopyPasteScore scores ch userId msg
  userMessageScoreAdult <- getUserAdultScore ch scores user
  userMessageScoreEmojiInName <- doesNameContainEmoji ch scores user
  let userMessageScore = UserMessageScore
        { userMessageScoreNoUsername = hasNoUsername scores user
        , userMessageScoreEmojiInName
        , userMessageScoreIsPremium = isPremiumScore scores user
        , userMessageScoreAdult
        , userMessageScoreKnownName = isKnownSpamerName scores user
        , userMessageScoreRichMarkup = messageContainsRichMarkup scores msg
        , userMessageScoreWords = messageWordsScore scores msg
        , userMessageScoreCopyPaste
        , userMessageScoreUsernameWords = messageUsernameWordsScore scores user
        , userMessageScoreQuotes = messageQuoteScore scores msg
        }
      totalScore = getTotalScore userMessageScore

      majorThreshold = fromIntegral @_ @Int $ scoreMajorThreshold scores
      criticalThreshold = fromIntegral @_ @Int $ scoreCriticalThreshold scores

      mkDecision = if totalScore < majorThreshold then RegularMessage else
        if majorThreshold <= totalScore && totalScore < criticalThreshold
          then ProbablySpamMessage
          else MostLikelySpamMessage
  
  pure $! mkDecision userMessageScore

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
