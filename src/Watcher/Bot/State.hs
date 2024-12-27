module Watcher.Bot.State where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.HashTable (HashTable)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime (..))
import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Katip
import Servant.Client (ClientEnv, ClientM, runClientM)
import System.IO (stdout)
import Telegram.Bot.API
import Telegram.Bot.API.Names

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.HashTable as CHT
import qualified Data.Set as Set
import qualified Data.Text as Text

import Watcher.Bot.Cache
import Watcher.Bot.Integration.CAS
import Watcher.Bot.Settings
import Watcher.Bot.State.Chat
import Watcher.Bot.State.User
import Watcher.Bot.Types
import Watcher.Orphans ()

-- | Telegram Model.
type Model = BotState

type Groups = HashMap ChatId ChatState

type Admins = HashMap UserId (HashSet (ChatId, Maybe Text))

type Users = HashMap UserId UserState

type SpamMessages = HashMap MessageText Int

type Events = Set TriggeredEvent

data BotState = BotState
  { botSettings :: Settings
  , clientEnv :: ClientEnv
  , requestLock :: MVar ()
  , adultEmoji :: HashSet Char
  , self :: TVar (Maybe UserInfo)
  , logEnv :: LogEnv
  , casClient :: Manager
  -- caches
  , groups :: TVar Groups
  , admins :: TVar Admins
  , users :: TVar Users
  , blocklist :: Blocklist
  , spamMessages :: TVar SpamMessages
  , eventSet :: TVar Events
  }

-- | Bot has its own state
newBotState :: Settings -> IO BotState
newBotState settings = do
  logEnv <- makeLogEnv
  admins <- newTVarIO HM.empty
  clientEnv <- defaultTelegramClientEnv (Token . botToken $ settings)
  groups <- newTVarIO HM.empty
  users <- newTVarIO HM.empty
  blocklist <- newBlocklist
  spamMessages <- newTVarIO HM.empty
  requestLock <- newMVar ()
  eventSet <- newTVarIO Set.empty
  let adultEmoji = Text.foldl' (flip HS.insert) HS.empty . scoreAdultEmoji . scores
        $! settings
  self <- newTVarIO Nothing
  casClient <- newCasClient
  pure BotState { botSettings = settings, .. }

importBotState :: Settings -> IO BotState
importBotState settings@Settings {..} = do
  let StorageSettings {..} = storage
      adultEmoji = Text.foldl' (flip HS.insert) HS.empty . scoreAdultEmoji $! scores

  clientEnv <- defaultTelegramClientEnv (Token botToken)
  requestLock <- newMVar ()
  self <- newTVarIO Nothing
  logEnv <- makeLogEnv
  casClient <- newCasClient

  admins <- importCache adminsPath
  groups <- importCache groupsPath
  users <- importCache usersPath
  blocklist <- importBlocklist blocklistPath
  spamMessages <- importCache spamMessagesPath
  eventSet <- importCache eventSetPath

  pure $ BotState { botSettings = settings, .. }

data BanState = BanState
  { bannedMessages :: HashSet MessageText
  , bannedChats :: HashSet ChatId
  } deriving (Eq, Show, Generic, FromDhall, ToDhall)

newBanState :: BanState
newBanState = BanState
  { bannedMessages = HS.empty
  , bannedChats = HS.empty
  }

data Blocklist = Blocklist
  { blocklistSpamerBans :: HashTable UserId BanState
  , blocklistSpamerUsernames :: TVar (HashMap Text UserId)
  }

data BlocklistStorage = BlocklistStorage
  { spamerBans :: HashMap UserId BanState
  , spamerUsernames :: HashMap Text UserId
  }
  deriving (Show, Generic, FromDhall, ToDhall)

instance Monoid BlocklistStorage where
  mempty = BlocklistStorage HM.empty HM.empty

instance Semigroup BlocklistStorage where
  a <> b = BlocklistStorage
    { spamerBans = spamerBans a <> spamerBans b
    , spamerUsernames = spamerUsernames a <> spamerUsernames b
    }

newBlocklist :: MonadIO m => m Blocklist
newBlocklist = liftIO do
  blocklistSpamerBans <- CHT.newWithDefaults 0
  blocklistSpamerUsernames <- newTVarIO HM.empty
  pure Blocklist {..}

importBlocklist :: FilePath -> IO Blocklist
importBlocklist path = storageToBlocklist =<< readCache =<< importCache path

storageToBlocklist :: MonadIO m => BlocklistStorage -> m Blocklist
storageToBlocklist BlocklistStorage {..} = liftIO do
  blocklistSpamerBans <- CHT.newWithDefaults (length spamerBans)
  blocklistSpamerUsernames <- newTVarIO spamerUsernames
  pure Blocklist {..}

blocklistToStorage :: MonadIO m => Blocklist -> m BlocklistStorage
blocklistToStorage Blocklist {..} = liftIO do
  kvs <- CHT.readAssocsIO blocklistSpamerBans
  let spamerBans = HM.fromList kvs
  spamerUsernames <- readCache blocklistSpamerUsernames
  pure BlocklistStorage {..}

alterBlocklist
  :: MonadIO m => Blocklist -> UserInfo -> (Maybe BanState -> Maybe BanState) -> m ()
alterBlocklist Blocklist{..} UserInfo{..} modifier = do
  liftIO do
    mBanState <- CHT.lookup blocklistSpamerBans userInfoId
    void $ case modifier mBanState of
      Nothing -> CHT.delete blocklistSpamerBans userInfoId
      Just v  -> CHT.insert blocklistSpamerBans userInfoId v 

  forM_ (userInfoUsername >>= normaliseUsername) \username ->
    writeCache blocklistSpamerUsernames username userInfoId

lookupBlocklist :: MonadIO m => Blocklist -> UserId -> m (Maybe BanState)
lookupBlocklist Blocklist{..} = liftIO . CHT.lookup blocklistSpamerBans

isDebugEnabled :: WithBotState => Bool
isDebugEnabled = let BotState{..} = ?model in debugEnabled botSettings

unlessDebug :: (WithBotState, Monad m) => m () -> m ()
unlessDebug action = unless isDebugEnabled $! action

withDebug :: (WithBotState, Monad m) => m () -> m ()
withDebug action = when isDebugEnabled $! action

withLock :: WithBotState => (Show a, MonadIO m) => ClientM a -> m (Maybe a)
withLock action = liftIO $ do
  let BotState{clientEnv, requestLock} = ?model
  takeMVar requestLock
  eResult <- flip runClientM clientEnv action
  case eResult of
    Left err -> log' err
    _ -> pure ()
  let mResult = either (const Nothing) Just eResult
  putMVar requestLock ()
  pure mResult

call :: WithBotState => (MonadIO m, Show a) => ClientM a -> m (Maybe a)
call = liftIO . callIO

callIO :: WithBotState => Show a => ClientM a -> IO (Maybe a)
callIO action = do
  response <- liftIO (withLock action)
  wait 1
  pure response

callCasCheck :: WithBotState => MonadIO m => UserId -> m (Maybe [Text])
callCasCheck userId = do
  let BotState{..} = ?model
      Settings{..} = botSettings

  eResponse <- liftIO $ checkUser casClient cas userId
  case eResponse of
    Left str -> liftIO (log' str) >> pure Nothing
    Right CasResponse{..} -> pure $
      if casResponseOk
        then casResultMessages <$> casResponseResult
        else Nothing    

wait :: Int -> IO ()
wait = threadDelay . (1_000_000 *)

data SelfDestructMessage = SelfDestructMessage
  { selfDestructMessageChatId :: ChatId
  , selfDestructMessageId :: MessageId
  , selfDestructMessageTime :: UTCTime
  }
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

instance Ord SelfDestructMessage where
  compare = comparing selfDestructMessageTime

data UserChatMemberCheck = UserChatMemberCheck
  { userChatMemberCheckChatId :: ChatId
  , userChatMemberCheckUserInfo :: UserInfo
  , userChatMemberCheckTime :: UTCTime
  }
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

instance Ord UserChatMemberCheck where
  compare = comparing userChatMemberCheckTime

data TriggeredEvent
  = SelfDestructMessageEvent SelfDestructMessage
  | UserChatMemberCheckEvent UserChatMemberCheck
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

instance Ord TriggeredEvent where
  compare = comparing triggeredEventTime

triggeredEventTime :: TriggeredEvent -> UTCTime
triggeredEventTime = \case
  SelfDestructMessageEvent SelfDestructMessage{..} -> selfDestructMessageTime
  UserChatMemberCheckEvent UserChatMemberCheck{..} -> userChatMemberCheckTime

makeLogEnv :: IO LogEnv
makeLogEnv = do
  handleScribe <- mkHandleScribe (ColorLog False) stdout (permitItem InfoS) V2
  registerScribe "stdout" handleScribe defaultScribeSettings
    =<< initLogEnv "Watcher" "production"

type WithBotState = (?model :: BotState)

log' :: WithBotState => Show a => a -> IO ()
log' x = do
  let BotState{logEnv} = ?model
      item = ()
      loc = Nothing
      severity = InfoS
  runKatipContextT logEnv item mempty $ do
    logItem item mempty loc severity (showLS x)

logT :: WithBotState => Text -> IO ()
logT x = do
  let BotState{logEnv} = ?model
      item = ()
      loc = Nothing
      severity = InfoS
  runKatipContextT logEnv item mempty $ do
    logItem item mempty loc severity (ls x)
