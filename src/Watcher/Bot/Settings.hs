{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Watcher.Bot.Settings where

import Data.Map.Strict (Map)
import Dhall

data SpamCommand
  = SCPoll
  | SCAdminsCall
  deriving (Eq, Show, Read, Generic, FromDhall, ToDhall)

spamCommandToText :: SpamCommand -> Text
spamCommandToText = \case
  SCPoll -> "Poll with consensus"
  SCAdminsCall -> "Call chat admins"

data GroupSettings = GroupSettings
  { usersForConsensus :: Integer -- ^ Amount of users needed to vote for ban.
  , spamCommandAction :: SpamCommand -- ^ Whether "/spam" enabled or not and what action should be performed.
  , messagesInQuarantine :: Integer -- ^ Amount of messages to keep users in quarantine.
  , selfDestroyEnabled :: Bool -- ^ Whether self-destructive messages enabled or not.
  } deriving (Generic, FromDhall, ToDhall, Show)

data OwnerGroupSettings = OwnerGroupSettings
  { ownerGroupId :: Integer -- ^ ChatId of super-admin group.
  , ownerGroupSpamThreadId :: Integer -- ^ MessageThreadId of spam topic.
  , ownerGroupTuningThreadId :: Integer -- ^ MessageThreadId of tuning topic.
  , ownerGroupFeedbackThreadId :: Integer -- ^ MessageThreadId of feedback topic.
  , ownerGroupStatsThreadId :: Integer -- ^ MessageThreadId of statistics topic.
  } deriving (Generic, FromDhall, ToDhall, Show)

data ScoreSettings = ScoreSettings
  { scoreUserHasNoUsername :: Natural
  , scoreUserNameContainsEmoji :: Natural
  , scoreUserHasPremium :: Natural
  , scoreUserAdultScore :: Natural
  , scoreUserKnownSpamerNames :: Map Text Natural
  , scoreMessageContainsRichMarkup :: Natural
   -- FIXME: Consider Frozen Dictionary
  , scoreMessageWordsScore :: Map Text Natural
  , scoreAdultEmoji :: Text
  , scoreMajorThreshold :: Natural
  , scoreCriticalThreshold :: Natural
  , scoreCopyPaste :: Natural
  } deriving (Generic, FromDhall, ToDhall, Show)
  
data HelpSettings = HelpSettings
  { ownerHelp :: Text
  , publicHelp :: Text
  , adminHelp :: Text
  } deriving (Generic, FromDhall, ToDhall, Show)

data StorageSettings = StorageSettings
  { groupsPath :: FilePath
  , adminsPath :: FilePath
  , usersPath :: FilePath
  , blocklistPath :: FilePath
  , spamMessagesPath :: FilePath
  , selfDestructionSetPath :: FilePath
  } deriving (Generic, FromDhall, ToDhall, Show)

data AnalyticsSettings = AnalyticsSettings
  { analyticsDir :: FilePath
  , chatEventsPath :: FilePath
  , userEventsPath :: FilePath
  , restEventsPath :: FilePath
  } deriving (Generic, FromDhall, ToDhall, Show)

data Settings = Settings
  { botName :: Text -- ^ Telegram bot name. Used to parse @/command\@botname@.
  , botToken :: Text -- ^ Bot token.
  , ownerGroup :: Maybe OwnerGroupSettings -- ^ Optional, super-admin group settings.
  , debugEnabled :: Bool -- ^ Whether debug enabled or not
  , defaultGroupSettings :: GroupSettings -- ^ Default group settings
  , scores :: ScoreSettings
  , helpSettings :: HelpSettings
  , storage :: StorageSettings
  , analytics :: AnalyticsSettings
  , workers :: WorkersSettings
  } deriving (Generic, FromDhall, ToDhall, Show)

data WorkersSettings = WorkersSettings
  { cleanup :: WorkerSettings
  , dump :: WorkerSettings
  , statistics :: WorkerSettings
  } deriving (Generic, FromDhall, ToDhall, Show)

data WorkerSettings = WorkerSettings
  { workerName :: Text
  , workerPeriod :: WorkerPeriod
  , workerPeriodUnits :: Natural
  } deriving (Generic, FromDhall, ToDhall, Show)

data WorkerPeriod = Second | Minute | Hour | Day | Week
  deriving (Generic, FromDhall, ToDhall, Show)

workerPeriodToSec :: WorkerPeriod -> Int
workerPeriodToSec = \case
  Second -> 1
  Minute -> 60
  Hour -> 60 * 60
  Day -> 60 * 60 * 24
  Week -> 60 * 60 * 24 * 7

-- | Load settings from file.
loadSettings :: Text -> IO Settings
loadSettings = input auto

-- | Load default settings.
loadDefaultSettings :: IO Settings
loadDefaultSettings = loadSettings "./config/settings.dhall"

load :: FromDhall a => Text -> IO a
load = input auto
