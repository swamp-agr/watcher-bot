module Watcher.Bot.Analytics where

import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Csv (FromRecord (..), FromField (..), ToRecord (..), ToField (..))
import Data.Text (Text)
import Data.Time (UTCTime (..))
import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Telegram.Bot.API
import Telegram.Bot.Simple

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv

import Watcher.Bot.Settings
import Watcher.Bot.State

data EventType
  = EventGroupAdd -- FIXME: catch it
  | EventGroupSetup
  | EventGroupSetupCompleted
  | EventUserSetup
  | EventGroupSpam
  | EventGroupRecogniseProbablySpam
  | EventGroupRecogniseMostLikelySpam
  | EventGroupBan
  | EventGroupUnban
  | EventLeaveUnsupported
  | EventUserPrivateCommand
  deriving (Eq, Show, Ord, Generic, FromDhall, ToDhall)

instance FromField EventType where
  parseField = \case
    "group_add" -> pure EventGroupAdd
    "group_setup" -> pure EventGroupSetup
    "group_setup_completed" -> pure EventGroupSetupCompleted
    "user_setup" -> pure EventUserSetup
    "spam" -> pure EventGroupSpam
    "probably_spam_recognised" -> pure EventGroupRecogniseProbablySpam
    "most_likely_spam_recognised" -> pure EventGroupRecogniseMostLikelySpam
    "ban" -> pure EventGroupBan
    "unban" -> pure EventGroupUnban
    "leave_unsupported" -> pure EventLeaveUnsupported
    "private_command" -> pure EventUserPrivateCommand
    _ -> mzero

instance ToField EventType where
  toField = \case
    EventGroupAdd -> "group_add"
    EventGroupSetup -> "group_setup"
    EventGroupSetupCompleted -> "group_setup_completed"
    EventUserSetup -> "user_setup"
    EventGroupSpam -> "spam"
    EventGroupRecogniseProbablySpam -> "probably_spam_recognised"
    EventGroupRecogniseMostLikelySpam -> "most_likely_spam_recognised"
    EventGroupBan -> "ban"
    EventGroupUnban -> "unban"
    EventLeaveUnsupported -> "leave_unsupported"      
    EventUserPrivateCommand -> "private_command"

data Event = Event
  { eventTime :: UTCTime
  , eventType :: EventType
  , eventChatId :: Maybe ChatId
  , eventUserId :: Maybe UserId
  , eventData :: Maybe Text
  } deriving (Show, Eq, Generic, ToRecord, FromRecord)

event :: UTCTime -> EventType -> Event
event time evtType = Event time evtType Nothing Nothing Nothing

chatEvent :: UTCTime -> ChatId -> EventType -> Event
chatEvent time chatId evtType = (event time evtType) { eventChatId = Just chatId }

userEvent :: UTCTime -> UserId -> EventType -> Event
userEvent time userId evtType = (event time evtType) { eventUserId = Just userId }

sendEvent :: WithBotState => Event -> BotM ()
sendEvent evt = do
  let BotState {..} = ?model
  let Settings {..} = botSettings
      AnalyticsSettings{..} = analytics

      reportPath = analyticsDir </> case eventType evt of
        EventGroupAdd -> chatEventsPath
        EventGroupSetup -> chatEventsPath
        EventGroupSetupCompleted -> chatEventsPath
        EventUserSetup -> userEventsPath
        EventGroupSpam -> chatEventsPath      
        EventGroupRecogniseProbablySpam -> chatEventsPath
        EventGroupRecogniseMostLikelySpam -> chatEventsPath
        EventGroupBan -> chatEventsPath
        EventGroupUnban -> chatEventsPath
        EventLeaveUnsupported -> restEventsPath
        EventUserPrivateCommand -> userEventsPath

  liftIO $! do
    createDirectoryIfMissing True analyticsDir
    BSL.appendFile reportPath $! Csv.encode (pure evt)
