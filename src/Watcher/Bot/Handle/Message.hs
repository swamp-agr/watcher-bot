module Watcher.Bot.Handle.Message where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Char (ord)
import Data.Foldable (asum)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Time (getCurrentTime)
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

handleAnalyseMessage :: BotState -> ChatId -> UserId -> Message -> BotM ()
handleAnalyseMessage model@BotState{..} chatId userId message = do
  mChatState <- lookupCache groups chatId
  forM_ mChatState $ \ch@ChatState{..} -> case chatSetup of
    SetupNone -> pure ()
    SetupInProgress {} -> pure ()
    SetupCompleted {} -> analyseMessage model chatId ch userId message

handleTuning :: BotState -> Update -> BotM ()
handleTuning model@BotState{..} Update{..} = do
  let mMsg = asum [ updateMessage, updateEditedMessage ]
  forM_ mMsg $ \origMsg -> do
    void $ call model
      $ deleteMessage (chatId $ messageChat origMsg) (messageMessageId origMsg)
    forM_ (messageReplyToMessage origMsg) $ \msg ->
      forM_ (messageForwardOrigin msg) $ \origin -> case origin of
        MessageOriginUser{messageOriginUserSenderUser} -> do
          let userId' = userId messageOriginUserSenderUser
              userChatId = coerce @_ @ChatId userId'
          mResponse <- call model $ getChat (SomeChatId userChatId)
          liftIO $ log' ("ChatFullInfo" :: Text, mResponse)
          let mChat = responseResult <$> mResponse
              ch = (newChatState botSettings)
                { quarantine = HM.singleton userId' (toChatInfo <$> mChat, Set.empty) }
              decision = decideAboutMessage model ch messageOriginUserSenderUser msg
          replyTuning (messageMessageId msg) (messageMessageThreadId msg) decision
        _ -> pure ()

analyseMessage :: BotState -> ChatId -> ChatState -> UserId -> Message -> BotM ()
analyseMessage model chatId ch userId message = do
  forM_ (messageNewChatMembers message) $ addToQuarantineOrBan model chatId ch

  let messageInfo = messageToMessageInfo message
  userAlreadyBanned <- hasUserAlreadyBannedElsewhere model userId
  if userAlreadyBanned
    -- if user has banned globally but was unbanned locally, bot will allow such a user
    then do
      liftIO $ log' ("analyseMessage: already banned" :: Text, message)
      unless (HS.member userId (allowlist ch)) $ do
        forM_ (messageFrom message) $ \spamerUser -> do
          let spamer = userToUserInfo spamerUser
          updateBlocklistAndMessages model chatId messageInfo
          banSpamerInChat model chatId spamer
          selfDestructReply model chatId ch (ReplyUserAlreadyBanned spamer)
    else do
      knownSpamMessage <- isKnownSpamMessage model messageInfo
      if knownSpamMessage
        then handleBanByRegularUser model chatId ch Nothing messageInfo
        else case userIsInChatQuarantine ch userId of
          Nothing -> pure ()
          Just inQuarantine -> forM_ (messageFrom message) $ \user -> do
            now <- liftIO getCurrentTime
            case decideAboutMessage model ch user message of
              RegularMessage _ -> incrementQuarantineCounter
                model chatId ch userId inQuarantine message
              ProbablySpamMessage _ ->
                sendEvent model (chatEvent now chatId EventGroupRecogniseProbablySpam)
              MostLikelySpamMessage _ -> do
                sendEvent model (chatEvent now chatId EventGroupRecogniseMostLikelySpam)
                forwardToOwnersMaybe model Spam chatId (messageInfoId messageInfo)
                handleBanByRegularUser model chatId ch Nothing messageInfo

incrementQuarantineCounter
  :: BotState -> ChatId -> ChatState -> UserId -> Int -> Message -> BotM ()
incrementQuarantineCounter
  BotState{..} chatId ch@ChatState{..} userId inQuarantine Message{..} = do
  forM_ messageText $ \txt -> do
    let GroupSettings{..} = chatSettings
        enoughToRelease = inQuarantine + 1 == fromIntegral messagesInQuarantine
        hash = hexSha256 txt
    if enoughToRelease
      then do
        let nextState = ch { quarantine = HM.delete userId quarantine }
        writeCache groups chatId nextState
      else do
        let go Nothing = Just $! (Nothing, Set.singleton hash)
            go (Just (chat, s)) = Just $! (chat, Set.insert hash s)
            nextState = ch { quarantine = HM.alter go userId quarantine }
        writeCache groups chatId nextState

userIsInChatQuarantine :: ChatState -> UserId -> Maybe Int
userIsInChatQuarantine ChatState{..} userId =
  HM.lookup userId quarantine >>= pure . Set.size . snd

addToQuarantineOrBan :: BotState -> ChatId -> ChatState -> [User] -> BotM ()
addToQuarantineOrBan model@BotState{..} chatId ch@ChatState{..} newcomers = do
  newcomersWithChats <- forM newcomers $ \newcomer -> do
    let uid = userId newcomer
    lookupCache blocklist uid >>= \case
      Just bs@BanState{..} -> do
        let nextBanState = bs { bannedChats = HS.insert chatId bannedChats }
            spamer = userToUserInfo newcomer
        writeCache blocklist uid nextBanState

        banSpamerInChat model chatId spamer
        selfDestructReply model chatId ch (ReplyUserAlreadyBanned spamer)

        pure Nothing

      Nothing -> do
        let userChatId = SomeChatId $ coerce @_ @ChatId uid
        mResponse <- call model $ getChat userChatId
        pure $ Just ((toChatInfo . responseResult) <$> mResponse, newcomer)

  let toQuarantineEntry (mChat, User{..}) = (userId, (mChat, Set.empty))
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
  ] <> Map.elems userMessageScoreWords

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

isKnownSpamMessage :: BotState -> MessageInfo -> BotM Bool
isKnownSpamMessage BotState{..} msg =
  maybe (pure False)
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
      let mChat = fst =<< HM.lookup (userId u) quarantine
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

getUserAdultScore :: BotState -> ChatState -> ScoreSettings -> User -> Int
getUserAdultScore BotState{..} ChatState{..} ScoreSettings{..} u =
  let go n c = n + if HS.member c adultEmoji then scoreUserAdultScore else 0
      computeScore txt = fromIntegral $ Text.foldl' go 0 txt
      getChatScore ChatInfo{..} = computeScore chatInfoName
        + maybe 0 computeScore chatInfoBio
      
      userScore = computeScore (getUserName u)
      mChat = fst =<< HM.lookup (userId u) quarantine
      chatScore = maybe 0 getChatScore mChat
  in userScore + chatScore

messageContainsRichMarkup :: ScoreSettings -> Message -> Int
messageContainsRichMarkup ScoreSettings{..}
  = (* (fromIntegral scoreMessageContainsRichMarkup))
  . length
  . filter ((== MessageEntityCustomEmoji) . messageEntityType)
  . fromMaybe [] . messageEntities

-- FIXME: use duckling
messageWordsScore :: ScoreSettings -> Message -> Map Text Int
messageWordsScore ScoreSettings{scoreMessageWordsScore} Message{messageText} =
  let go wordMap word = ((word, ) . fromIntegral) <$> Map.lookup word wordMap
  
      getTotalWordsScore =
        Map.fromListWith (+)
        . catMaybes
        . fmap (go scoreMessageWordsScore . Text.toLower)
        . Text.words
  in getTotalWordsScore $! fromMaybe "" messageText

messageCopyPasteScore :: ScoreSettings -> ChatState -> UserId -> Message -> Int
messageCopyPasteScore ScoreSettings{..} ChatState{..} userId Message{..} = 
  let messages = fromMaybe Set.empty $! fmap snd $! HM.lookup userId quarantine
  in if Set.null messages
    then 0
    else let messageExist = fromMaybe False
               (flip Set.member messages . hexSha256 <$> messageText)
         in if messageExist
            then fromIntegral scoreCopyPaste
            else 0

decideAboutMessage
  :: BotState -> ChatState -> User -> Message -> MessageDecision
decideAboutMessage model@BotState{..} ch user@User{userId} msg =
  let Settings{..} = botSettings
      
      userMessageScore = UserMessageScore
        { userMessageScoreNoUsername = hasNoUsername scores user
        , userMessageScoreEmojiInName = doesNameContainEmoji ch scores user
        , userMessageScoreIsPremium = isPremiumScore scores user
        , userMessageScoreAdult = getUserAdultScore model ch scores user
        , userMessageScoreKnownName = isKnownSpamerName scores user
        , userMessageScoreRichMarkup = messageContainsRichMarkup scores msg
        , userMessageScoreWords = messageWordsScore scores msg
        , userMessageScoreCopyPaste = messageCopyPasteScore scores ch userId msg
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
