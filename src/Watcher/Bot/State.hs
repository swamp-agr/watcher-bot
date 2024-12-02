module Watcher.Bot.State where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Exception.Safe (SomeException, try)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime (..))
import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)
import Servant.Client (ClientEnv, ClientM, runClientM)
import Telegram.Bot.API
import Telegram.Bot.Simple (BotM, liftClientM)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Set as Set
import qualified Data.Text as Text

import Watcher.Bot.Cache
import Watcher.Bot.Settings
import Watcher.Bot.State.Chat
import Watcher.Bot.State.User
import Watcher.Bot.Types
import Watcher.Bot.Utils
import Watcher.Orphans ()

-- | Telegram Model.
type Model = BotState

data BotState = BotState
  { botSettings :: Settings
  , clientEnv :: ClientEnv
  , requestLock :: MVar ()
  , adultEmoji :: HashSet Char
  , self :: TVar (Maybe UserInfo)
  -- caches
  , groups :: TVar (HashMap ChatId ChatState)
  , admins :: TVar (HashMap UserId (HashSet (ChatId, Maybe Text)))
  , users :: TVar (HashMap UserId UserState)
  , blocklist :: TVar (HashMap UserId BanState)
  , spamMessages :: TVar (HashMap MessageText Int)
  , selfDestructionSet :: TVar (Set SelfDestructMessage)
  }

-- | Bot has its own state
newBotState :: Settings -> IO BotState
newBotState settings = do
  admins <- newTVarIO HM.empty
  clientEnv <- defaultTelegramClientEnv (Token . botToken $ settings)
  groups <- newTVarIO HM.empty
  users <- newTVarIO HM.empty
  blocklist <- newTVarIO HM.empty
  spamMessages <- newTVarIO HM.empty
  requestLock <- newMVar ()
  selfDestructionSet <- newTVarIO Set.empty
  let adultEmoji = Text.foldl' (flip HS.insert) HS.empty . scoreAdultEmoji . scores
        $! settings
  self <- newTVarIO Nothing
  pure BotState { botSettings = settings, .. }

importBotState :: Settings -> IO BotState
importBotState settings@Settings {..} = do
  let StorageSettings {..} = storage
      adultEmoji = Text.foldl' (flip HS.insert) HS.empty . scoreAdultEmoji $! scores

  clientEnv <- defaultTelegramClientEnv (Token botToken)
  requestLock <- newMVar ()
  self <- newTVarIO Nothing

  admins <- importCache adminsPath
  groups <- importCache groupsPath
  users <- importCache usersPath
  blocklist <- importCache blocklistPath
  spamMessages <- importCache spamMessagesPath
  selfDestructionSet <- importCache selfDestructionSetPath

  pure $ BotState { botSettings = settings, .. }

isDebugEnabled :: BotState -> Bool
isDebugEnabled = debugEnabled . botSettings

unlessDebug :: Monad m => BotState -> m () -> m ()
unlessDebug model action = unless (isDebugEnabled model) $! action

withDebug :: Monad m => BotState -> m () -> m ()
withDebug model action = when (isDebugEnabled model) $! action

withLock :: (Show a, MonadIO m) => BotState -> ClientM a -> m (Maybe a)
withLock BotState{clientEnv, requestLock} action = liftIO $ do
  takeMVar requestLock
  eResult <- flip runClientM clientEnv action
  case eResult of
    Left err -> log' err
    _ -> pure ()
  let mResult = either (const Nothing) Just eResult
  putMVar requestLock ()
  pure mResult

withDelay :: ClientM (Maybe a) -> BotM (Maybe a)
withDelay action = do
  result <- liftClientM $ try action >>= \case
      Left (e' :: SomeException) -> liftIO $ log' e' >> pure Nothing
      Right result -> pure $! result
  liftIO (wait 1) -- FIXME: make configurable
  pure result

call :: Show a => BotState -> ClientM a -> BotM (Maybe a)
call model action = withDelay $ withLock model action

data BanState = BanState
  { bannedMessages :: HashSet MessageText
  , bannedChats :: HashSet ChatId
  } deriving (Eq, Show, Generic, FromDhall, ToDhall)

wait :: Int -> IO ()
wait = threadDelay . (1_000_000 *)

newBanState :: BanState
newBanState = BanState
  { bannedMessages = HS.empty
  , bannedChats = HS.empty
  }

data SelfDestructMessage = SelfDestructMessage
  { selfDestructMessageChatId :: ChatId
  , selfDestructMessageId :: MessageId
  , selfDestructMessageTime :: UTCTime
  }
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

instance Ord SelfDestructMessage where
  compare = comparing selfDestructMessageTime

