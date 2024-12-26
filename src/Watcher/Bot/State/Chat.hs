module Watcher.Bot.State.Chat where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (Day, UTCTime (..))
import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)
import Telegram.Bot.API

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as Text

import Watcher.Bot.Settings
import Watcher.Bot.Types
import Watcher.Bot.Utils

data ChatState = ChatState
  { chatSettings :: GroupSettings
  , chatAdmins :: HashSet UserId
  , chatAdminsCheckedAt :: Maybe Day
  , chatSetup :: SetupState
  , quarantine :: HashMap UserId QuarantineState
  , activePolls :: HashMap SpamerId PollState -- ^ key is candidate for ban in given group, value is a set of unique voters in favour of ban and a message with poll.
  , adminCalls :: HashMap SpamerId (UserInfo, MessageInfo)
  , allowlist :: HashSet UserId
  , botIsAdmin :: Bool
  }
  deriving (Show, Generic, FromDhall, ToDhall)

newChatState :: Settings -> ChatState
newChatState Settings{..} = ChatState
  { chatSettings = defaultGroupSettings
  , chatAdmins = HS.empty
  , chatAdminsCheckedAt = Nothing
  , chatSetup = SetupNone
  , quarantine = HM.empty
  , activePolls = HM.empty
  , adminCalls = HM.empty
  , allowlist = HS.empty
  , botIsAdmin = False
  }

-- | State of the chat setup.
-- Consider setup as an interruptible process with a lock and a given timeout.
-- If setup was initiated long ago and incomplete (i.e. InProgress), it could be overriden
-- by some other admin. Admin could explicitly free the lock on setup by completing setup.
data SetupState
  = SetupNone
  | SetupInProgress
      { setupModifiedByAdmin :: UserId
      , setupModifiedAt :: UTCTime
      }
  | SetupCompleted
      { setupCompletedByAdmin :: UserId
      , setupCompletedAt :: UTCTime
      }
  deriving (Show, Generic, FromDhall, ToDhall)

setupChatSettings
  :: ChatState
  -> UserId
  -> UTCTime
  -> GroupSettings
  -> ChatState
setupChatSettings st userId time newSettings = st
  { chatSetup = SetupInProgress { setupModifiedByAdmin = userId, setupModifiedAt = time }
  , chatSettings = newSettings
  }

startBanPoll
  :: ChatState
  -> Maybe VoterId
  -> SpamerId
  -> UserInfo -- ^ Spamer
  -> MessageId -- ^ poll message id
  -> MessageId -- ^ spamer message id
  -> (PollState, ChatState)
startBanPoll st@ChatState{..} mVoterId spamerId pollSpamer pollMessageId pollSpamMessageId =
  let newPoll = PollState
        { pollMessageId, pollSpamer, pollSpamMessageId
        , pollVoters = maybe HS.empty HS.singleton mVoterId
        }
      poll = case HM.lookup spamerId activePolls of
        Nothing -> newPoll
        Just oldPoll -> oldPoll
          { pollVoters = maybe HS.empty (flip HS.insert (pollVoters oldPoll)) mVoterId
          }
  in (poll, st { activePolls = HM.insert spamerId poll activePolls })

renderChatState :: ChatState -> Text
renderChatState ChatState{..} = Text.unlines
  [ "Current group settings are:"
  , ""
  , "Users for consensus: " <> s2t usersForConsensus
  , "Action on `/spam` command: " <> spamCommandToText spamCommandAction
  , "Quarantine duration (in messages): " <> s2t messagesInQuarantine
  , "Is Bot Admin Already? " <> if botIsAdmin then "✔️" else "❌"
  , "Send Self-destroyable messages: " <> if selfDestroyEnabled then "✔️" else "❌"
  ]
  where
    GroupSettings {..} = chatSettings

addVoteToPoll
  :: ChatState
  -> VoterId -- ^ Voter
  -> SpamerId -- ^ Spamer candidate
  -> PollState
  -> (PollState, ChatState)
addVoteToPoll st@ChatState{..} voterId spamerId poll =
  let nextPoll = case HM.lookup spamerId activePolls of
        Nothing -> poll { pollVoters = HS.insert voterId (pollVoters poll) }
        Just ps@PollState{..} -> ps { pollVoters = HS.insert voterId pollVoters }
  in (nextPoll, st { activePolls = HM.insert spamerId nextPoll activePolls })

data PollState = PollState
  { pollMessageId :: MessageId
  , pollVoters :: HashSet VoterId
  , pollSpamer :: UserInfo
  , pollSpamMessageId :: MessageId
  }
  deriving (Show, Eq, Generic, FromDhall, ToDhall)

data QuarantineState = QuarantineState
  { quarantineUserChatInfo :: Maybe ChatInfo
  , quarantineMessageHash :: Set MessageHash
  , quarantineMessageId :: Set MessageId
  }
  deriving (Show, Eq, Generic, FromDhall, ToDhall)

emptyQuarantineState :: QuarantineState
emptyQuarantineState = QuarantineState
  { quarantineUserChatInfo = Nothing
  , quarantineMessageHash = mempty
  , quarantineMessageId = mempty
  }
