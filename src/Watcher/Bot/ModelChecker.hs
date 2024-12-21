module Watcher.Bot.ModelChecker where

import Control.Monad (forM_, when)
import Data.Aeson (FromJSON (..), (.:), (.:?), eitherDecodeFileStrict', withObject)
import Data.Aeson.Types (Object, Parser)
import Data.Csv (ToRecord (..), ToField (..), record)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)
import Telegram.Bot.API
import Telegram.Bot.API.Internal.Utils (gparseJSON)
import Text.Read (readMaybe)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Watcher.Bot.Handle.Message
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.State.Chat

data ExportedChat = ExportedChat
  { exportedChatId :: Integer
  , exportedChatName :: Text
  , exportedChatType :: Text
  , exportedChatMessages :: [ExportedMessage]
  }
  deriving (Eq, Generic, Show)

instance FromJSON ExportedChat where parseJSON = gparseJSON

data ExportedMessage
  = ExportedService
      { exportedServiceId :: Integer
      , exportedServiceType :: Text
      , exportedServiceDate :: POSIXTime
      , exportedServiceActor :: Maybe Text
      , exportedServiceAction :: Text
      }
  | ExportedMessage
      { exportedMessageId :: Integer
      , exportedMessageType :: Text
      , exportedMessageDate :: POSIXTime
      , exportedMessageFrom :: Maybe Text
      , exportedMessageFromId :: Maybe Integer
      , exportedMessageTextEntities :: Maybe [ExportTextEntity]
      }
  deriving (Eq, Show, Generic)

instance FromJSON ExportedMessage where
  parseJSON = withObject "ExportedMessage" \o ->
    (o .: "type" :: Parser Text) >>= \case
    "service" -> ExportedService
      <$> o .: "id"
      <*> o .: "type"
      <*> dateUnixtimeParser o
      <*> o .:? "actor"
      <*> o .: "action"
    "message" -> ExportedMessage
      <$> o .: "id"
      <*> o .: "type"
      <*> dateUnixtimeParser o
      <*> o .:? "from"
      <*> fromChatIdParser o
      <*> o .:? "text_entities"
    x -> fail $ "Unrecognised message type: " <> Text.unpack x
        
fromChatIdParser :: Object -> Parser (Maybe Integer)
fromChatIdParser o = do
  chatIdText <- o .:? "from_id"
  case fmap (readMaybe . Text.unpack . Text.drop 4) chatIdText of
    Nothing -> fail $ "from_id: cannot parse" <> show chatIdText
    Just chatId -> pure chatId

dateUnixtimeParser :: Object -> Parser POSIXTime
dateUnixtimeParser o = do
  messageTime <- o .: "date_unixtime"
  case readMaybe messageTime of
    Nothing -> fail $ "date_unixtime: cannot parse " <> messageTime
    Just time -> pure $ fromIntegral @Integer @POSIXTime time

data ExportTextEntity = ExportTextEntity
  { exportTextEntityType :: Text
  , exportTextEntityText :: Maybe Text
  }
  deriving (Eq,Show, Generic)

instance FromJSON ExportTextEntity where parseJSON = gparseJSON

getExportedMessageId :: ExportedMessage -> Integer
getExportedMessageId = \case
  ExportedService{exportedServiceId} -> exportedServiceId
  ExportedMessage{exportedMessageId} -> exportedMessageId

data ChatExportTuningEntry = ChatExportTuningEntry
 { chatExportTuningEntryChatId :: Integer
 , chatExportTuningEntryMessageId :: Integer
 , chatExportTuningEntryUserId :: Integer
 , chatExportTuningEntryMessageText :: Text
 , chatExportTuningEntryDecision :: MessageDecision
 }
 deriving (Eq, Show, Generic)

instance ToRecord ChatExportTuningEntry where
  toRecord ChatExportTuningEntry {..} = record $
    [ toField chatExportTuningEntryChatId
    , toField chatExportTuningEntryMessageId
    , toField chatExportTuningEntryUserId
    , toField chatExportTuningEntryMessageText
    , toField (messageDecisionToText chatExportTuningEntryDecision)
    , toField userMessageScoreNoUsername
    , toField userMessageScoreEmojiInName
    , toField userMessageScoreIsPremium
    , toField userMessageScoreAdult
    , toField userMessageScoreKnownName
    , toField userMessageScoreRichMarkup
    , toField userMessageScoreCopyPaste
    , toField (sum $ (0 : Map.elems userMessageScoreWords))
    ]
    where
      UserMessageScore {..} = messageDecisionToScore chatExportTuningEntryDecision

processChatExport :: FilePath -> FilePath -> Maybe Int -> IO ()
processChatExport inputFile outputFile mFromId = do
  botState <- newBotState =<< loadDefaultSettings
  let ?model = botState
  eitherDecodeFileStrict' inputFile >>= \case
    Left err -> log' err
    Right result -> tuneChatExport outputFile mFromId result

tuneChatExport :: WithBotState => FilePath -> Maybe Int -> ExportedChat -> IO ()
tuneChatExport file mFromId ExportedChat{..} = do
  let BotState {..} = ?model
  when (isNothing mFromId) $ refreshFile file
  let fromMessageId msgId = dropWhile ((<= msgId) . getExportedMessageId) exportedChatMessages
      chatMessages = maybe exportedChatMessages (fromMessageId . fromIntegral) mFromId
  forM_ chatMessages $ \case
    ExportedService {} -> pure ()
    ExportedMessage {..} -> forM_ exportedMessageFromId $ \userId -> do
      let user = User
            { userId = UserId userId
            , userIsBot = False
            , userFirstName = fromMaybe "Deleted account" exportedMessageFrom
            , userLastName = Nothing
            , userUsername = Just "missing_username"
            , userLanguageCode = Nothing
            , userIsPremium = Nothing
            , userAddedToAttachmentMenu = Nothing
            , userCanJoinGroups = Nothing
            , userCanReadAllGroupMessages = Nothing
            , userSupportsInlineQueries = Nothing
            }
          chat = Chat
            { chatId = ChatId exportedChatId
            , chatType = ChatTypeSupergroup
            , chatTitle = Just exportedChatName
            , chatUsername = Just "missing_title"
            , chatFirstName = Just exportedChatName
            , chatLastName = Nothing
            , chatIsForum = Nothing
            }
          message = Message
            { messageMessageId = MessageId exportedMessageId
            , messageMessageThreadId = Nothing
            , messageFrom = Just user
            , messageSenderChat = Nothing
            , messageSenderBoostCount = Nothing
            , messageSenderBusinessBot = Nothing
            , messageDate = exportedMessageDate
            , messageBusinessConnectionId = Nothing
            , messageChat = chat
            , messageForwardOrigin = Nothing
            , messageIsTopicMessage = Nothing
            , messageIsAutomaticForward = Nothing
            , messageReplyToMessage = Nothing
            , messageExternalReply = Nothing
            , messageQuote = Nothing
            , messageReplyToStory = Nothing
            , messageViaBot = Nothing
            , messageEditDate = Nothing
            , messageHasProtectedContent = Nothing
            , messageIsFromOffline = Nothing
            , messageMediaGroupId = Nothing
            , messageAuthorSignature = Nothing
            , messageText = Just txt
            , messageEntities = Nothing
            , messageLinkPreviewOptions = Nothing
            , messageEffectId = Nothing
            , messageAnimation = Nothing
            , messageAudio = Nothing
            , messageDocument = Nothing
            , messagePhoto = Nothing
            , messageSticker = Nothing
            , messageStory = Nothing
            , messageVideo = Nothing
            , messageVideoNote = Nothing
            , messageVoice = Nothing
            , messageCaption = Nothing
            , messageCaptionEntities = Nothing
            , messageShowCaptionAboveMedia = Nothing
            , messageHasMediaSpoiler = Nothing
            , messageContact = Nothing
            , messageDice = Nothing
            , messageGame = Nothing
            , messagePoll = Nothing
            , messageVenue = Nothing
            , messageLocation = Nothing
            , messageNewChatMembers = Nothing
            , messageLeftChatMember = Nothing
            , messageNewChatTitle = Nothing
            , messageNewChatPhoto = Nothing
            , messageDeleteChatPhoto = Nothing
            , messageGroupChatCreated = Nothing
            , messageSupergroupChatCreated = Nothing
            , messageChannelChatCreated = Nothing
            , messageAutoDeleteTimerChanged = Nothing
            , messageHasAggressiveAntiSpamEnabled = Nothing
            , messageHasHiddenMembers = Nothing
            , messageMigrateToChatId = Nothing
            , messageMigrateFromChatId = Nothing
            , messagePinnedMessage = Nothing
            , messageInvoice = Nothing
            , messageSuccessfulPayment = Nothing
            , messageUsersShared = Nothing
            , messageChatShared = Nothing
            , messageConnectedWebsite = Nothing
            , messageWriteAccessAllowed = Nothing
            , messagePassportData = Nothing
            , messageProximityAlertTriggered = Nothing
            , messageBoostAdded = Nothing
            , messageChatBackgroundSet = Nothing
            , messageForumTopicCreated = Nothing
            , messageForumTopicEdited = Nothing
            , messageForumTopicClosed = Nothing
            , messageForumTopicReopened = Nothing
            , messageGeneralForumTopicHidden = Nothing
            , messageGeneralForumTopicUnhidden = Nothing
            , messageGiveawayCreated = Nothing
            , messageGiveaway = Nothing
            , messageGiveawayWinners = Nothing
            , messageGiveawayCompleted = Nothing
            , messageVideoChatScheduled = Nothing
            , messageVideoChatStarted = Nothing
            , messageVideoChatEnded = Nothing
            , messageVideoChatParticipantsInvited = Nothing
            , messageWebAppData = Nothing
            , messageReplyMarkup = Nothing
            }
          ch = newChatState botSettings
          decision = decideAboutMessage ch user message
          txt = Text.unwords $ catMaybes $ fmap exportTextEntityText $ fromMaybe [] exportedMessageTextEntities 
          entry = ChatExportTuningEntry
            { chatExportTuningEntryChatId = exportedChatId
            , chatExportTuningEntryMessageId = exportedMessageId
            , chatExportTuningEntryUserId = userId
            , chatExportTuningEntryMessageText = txt
            , chatExportTuningEntryDecision = decision
            }
      when (0 < getTotalScore (messageDecisionToScore decision)) $ appendEntry file entry

appendEntry :: FilePath -> ChatExportTuningEntry -> IO ()
appendEntry file = BSL.appendFile file . Csv.encode . pure

refreshFile :: FilePath -> IO ()
refreshFile file = writeFile file ""
