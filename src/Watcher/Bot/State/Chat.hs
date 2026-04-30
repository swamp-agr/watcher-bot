module Watcher.Bot.State.Chat where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (Day, UTCTime (..))
import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)
import Telegram.Bot.API

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Hashtables as HT
import qualified Data.Text as Text

import Watcher.Bot.Settings
import Watcher.Bot.Types
import Watcher.Bot.Utils

data ChatStorage = ChatStorage
  { chatSettings :: GroupSettings
  , chatAdmins :: HS.HashSet UserId
  , chatAdminsCheckedAt :: Maybe Day
  , chatSetup :: SetupState
  , quarantine :: HM.HashMap UserId QuarantineState
  , activePolls :: HM.HashMap SpamerId PollState -- ^ key is candidate for ban in given group, value is a set of unique voters in favour of ban and a message with poll.
  , adminCalls :: HM.HashMap SpamerId (UserInfo, MessageInfo)
  , allowlist :: HS.HashSet UserId
  , botIsAdmin :: Bool
  }
  deriving (Show, Generic, FromDhall, ToDhall)

data ChatState = ChatState
  { chatStateSettings :: GroupSettings
  , chatStateAdmins :: HashSet UserId
  , chatStateAdminsCheckedAt :: Maybe Day
  , chatStateSetup :: SetupState
  , chatStateQuarantine :: HashMap UserId QuarantineState
  , chatStateActivePolls :: HashMap SpamerId PollState -- ^ key is candidate for ban in given group, value is a set of unique voters in favour of ban and a message with poll.
  , chatStateAdminCalls :: HashMap SpamerId (UserInfo, MessageInfo)
  , chatStateAllowlist :: HashSet UserId
  , chatStateBotIsAdmin :: Bool
  }

newChatState :: MonadIO m => Settings -> m ChatState
newChatState = storageToChatState . newChatStorage

newChatStorage :: Settings -> ChatStorage
newChatStorage Settings{..} = ChatStorage
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

storageToChatState :: MonadIO m => ChatStorage -> m ChatState
storageToChatState ChatStorage{..} = liftIO do
  let chatStateSettings = chatSettings
      chatStateAdminsCheckedAt = chatAdminsCheckedAt
      chatStateSetup = chatSetup
      chatStateBotIsAdmin = botIsAdmin
  chatStateAdmins <- toHSet chatAdmins
  chatStateQuarantine <- toHMap quarantine
  chatStateActivePolls <- toHMap activePolls
  chatStateAdminCalls  <- toHMap adminCalls
  chatStateAllowlist <- toHSet allowlist
  pure ChatState{..}

chatToStorage :: MonadIO m => ChatState -> m ChatStorage
chatToStorage ChatState{..} = liftIO do
  let chatSettings = chatStateSettings
      chatAdminsCheckedAt = chatStateAdminsCheckedAt
      chatSetup = chatStateSetup
      botIsAdmin = chatStateBotIsAdmin
  chatAdmins <- fromHSet chatStateAdmins
  quarantine <- fromHMap chatStateQuarantine
  activePolls <- fromHMap chatStateActivePolls
  adminCalls <- fromHMap chatStateAdminCalls
  allowlist <- fromHSet chatStateAllowlist
  pure ChatStorage {..}

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
  { chatStateSetup =
      SetupInProgress { setupModifiedByAdmin = userId, setupModifiedAt = time }
  , chatStateSettings = newSettings
  }

startBanPoll
  :: MonadIO m
  => ChatState
  -> Maybe VoterId
  -> SpamerId
  -> UserInfo -- ^ Spamer
  -> MessageId -- ^ poll message id
  -> MessageId -- ^ spamer message id
  -> m (PollState, ChatState)
startBanPoll
  st@ChatState{..} mVoterId spamerId pollSpamer pollMessageId pollSpamMessageId = liftIO do
  let newPoll = PollState
        { pollMessageId, pollSpamer, pollSpamMessageId
        , pollVoters = maybe HS.empty HS.singleton mVoterId
        }
  poll <- HT.lookup chatStateActivePolls spamerId >>= \case
    Nothing -> pure newPoll
    Just oldPoll -> pure $! oldPoll
      { pollVoters = maybe HS.empty (flip HS.insert (pollVoters oldPoll)) mVoterId }
  HT.insert chatStateActivePolls spamerId poll
  pure (poll, st)

renderChatState :: ChatState -> Text
renderChatState ChatState{..} = Text.unlines
  [ "Current group settings are:"
  , ""
  , "Users for consensus: " <> s2t usersForConsensus
  , "Action on `/spam` command: " <> spamCommandToText spamCommandAction
  , "Quarantine duration (in messages): " <> s2t messagesInQuarantine
  , "Is Bot Admin Already? " <> if chatStateBotIsAdmin then "✔️" else "❌"
  , "Send Self-destroyable messages: " <> if selfDestroyEnabled then "✔️" else "❌"
  ]
  where
    GroupSettings {..} = chatStateSettings

addVoteToPoll
  :: MonadIO m
  => ChatState
  -> VoterId -- ^ Voter
  -> SpamerId -- ^ Spamer candidate
  -> PollState
  -> m (PollState, ChatState)
addVoteToPoll st@ChatState{..} voterId spamerId poll = liftIO do
  nextPoll <- HT.lookup chatStateActivePolls spamerId >>= \case
    Nothing -> pure
      $! poll { pollVoters = HS.insert voterId (pollVoters poll) }
    Just ps@PollState{..} -> pure $! ps { pollVoters = HS.insert voterId pollVoters }
  HT.insert chatStateActivePolls spamerId nextPoll
  pure (nextPoll, st)

data PollState = PollState
  { pollMessageId :: MessageId
  , pollVoters :: HS.HashSet VoterId
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

instance Monoid QuarantineState where
  mempty = emptyQuarantineState

instance Semigroup QuarantineState where
  a <> b = QuarantineState
    { quarantineUserChatInfo = quarantineUserChatInfo a <|> quarantineUserChatInfo b
    , quarantineMessageHash = quarantineMessageHash a <> quarantineMessageHash b
    , quarantineMessageId = quarantineMessageId a <> quarantineMessageId b
    }
