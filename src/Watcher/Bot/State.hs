module Watcher.Bot.State where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (fromException)
import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime (..))
import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( Manager, HttpException(..), HttpExceptionContent(..)
  , managerConnCount, managerModifyRequest
  , managerResponseTimeout, newManager, requestHeaders, responseTimeoutMicro
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hConnection)
import Katip
import Servant.Client (ClientEnv, ClientError (..), ClientM, mkClientEnv, runClientM)
import System.IO (stdout)
import Telegram.Bot.API
import Telegram.Bot.API.Names

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector.Hashtables as HT

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

type GroupStorage = HM.HashMap ChatId ChatStorage

transformChatImports
  :: MonadIO m => GroupStorage -> m Groups
transformChatImports xs = liftIO (sequenceA (HM.map storageToChatState xs) >>= toHMap)

transformChatExports
  :: MonadIO m => Groups -> m GroupStorage
transformChatExports xs = liftIO (fromHMap xs >>= sequenceA . HM.map chatToStorage)

type Admins = HashMap UserId (HashSet (ChatId, Maybe Text))

type AdminStorage = HM.HashMap UserId (HS.HashSet (ChatId, Maybe Text))

transformAdminImports :: MonadIO m => AdminStorage -> m Admins
transformAdminImports xs = liftIO (sequenceA (HM.map toHSet xs) >>= toHMap)

transformAdminExports :: MonadIO m => Admins -> m AdminStorage
transformAdminExports xs = liftIO (fromHMap xs >>= sequenceA . HM.map fromHSet)

type Users = HashMap UserId UserState

type UserStorage = HM.HashMap UserId UserState

type SpamMessages = HashMap MessageText Int

type SpamMessageStorage = HM.HashMap MessageText Int

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
  , groups :: Groups
  , admins :: TVar Admins
  , users :: TVar Users
  , blocklist :: TVar Blocklist
  , spamMessages :: TVar SpamMessages
  , eventSet :: TVar Events
  }

-- | Bot has its own state
newBotState :: Settings -> IO BotState
newBotState settings@Settings{..} = do
  logEnv <- makeLogEnv
  admins <- newTVarIO =<< HT.initialize 0
  clientEnv <- createTelegramClientEnv
    (Token botToken) (fromIntegral botConnectionCount) (fromIntegral botResponseTimeout)
  groups <- HT.initialize 0
  users <- newTVarIO =<< HT.initialize 0
  blocklist <- newTVarIO =<< storageToBlocklist mempty
  spamMessages <- newTVarIO =<< HT.initialize 0
  requestLock <- newMVar ()
  eventSet <- newTVarIO Set.empty
  let adultEmojiSet = Text.foldl' (flip HS.insert) HS.empty . scoreAdultEmoji $! scores
  adultEmoji <- toHSet adultEmojiSet
  self <- newTVarIO Nothing
  casClient <- newCasClient
  pure BotState { botSettings = settings, .. }

importBotState :: Settings -> IO BotState
importBotState settings@Settings {..} = do
  let StorageSettings {..} = storage
      adultEmojiSet = Text.foldl' (flip HS.insert) HS.empty . scoreAdultEmoji $! scores
  adultEmoji <- toHSet adultEmojiSet
  clientEnv <- createTelegramClientEnv
    (Token botToken) (fromIntegral botConnectionCount) (fromIntegral botResponseTimeout)
  requestLock <- newMVar ()
  self <- newTVarIO Nothing
  logEnv <- makeLogEnv
  casClient <- newCasClient

  admins <- newTVarIO =<< transformAdminImports =<< importCache adminsPath
  groups <- transformChatImports =<< importCache groupsPath
  users <- newTVarIO =<< toHMap =<< importCache usersPath
  blocklist <- newTVarIO =<< storageToBlocklist =<< importCache blocklistPath
  spamMessages <- newTVarIO =<< toHMap =<< importCache spamMessagesPath
  eventSet <- newTVarIO =<< importCache eventSetPath

  pure $ BotState { botSettings = settings, .. }

createTelegramClientEnv :: Token -> Int -> Int -> IO ClientEnv
createTelegramClientEnv token connCount timeoutSec = do
  let respTimeout = responseTimeoutMicro (timeoutSec * 1_000_000)

      addKeepAliveMaybe [] = [(hConnection, "keep-alive")]
      addKeepAliveMaybe hdrs = if (Map.member hConnection . Map.fromList) hdrs
        then hdrs
        else (hConnection, "keep-alive") : hdrs

      modifyRequest req = pure
        (req { requestHeaders = addKeepAliveMaybe (requestHeaders req) })

  mkClientEnv
    <$> newManager
       (tlsManagerSettings
         { managerResponseTimeout = respTimeout
         , managerConnCount = connCount
         , managerModifyRequest = modifyRequest
         })
    <*> pure (botBaseUrl token)

data BanState = BanState
  { banStateMessages :: HashSet MessageText
  , banStateChats :: HashSet ChatId
  }

data Blocklist = Blocklist
  { blocklistSpamerBans :: HashMap UserId BanState
  , blocklistSpamerUsernames :: HashMap Text UserId
  }

data BanStateStorage = BanStateStorage
  { bannedMessages :: HS.HashSet MessageText
  , bannedChats :: HS.HashSet ChatId
  } deriving (Eq, Show, Generic, FromDhall, ToDhall)

instance Semigroup BanStateStorage where
  a <> b = BanStateStorage
    { bannedMessages = HS.union (bannedMessages a) (bannedMessages b)
    , bannedChats = HS.union (bannedChats a) (bannedChats b)
    }

instance Monoid BanStateStorage where
  mempty = newBanStateStorage

newBanState :: IO BanState
newBanState = storageToBanState newBanStateStorage

newBanStateStorage :: BanStateStorage
newBanStateStorage = BanStateStorage
  { bannedMessages = HS.empty
  , bannedChats = HS.empty
  }

storageToBanState :: BanStateStorage -> IO BanState
storageToBanState BanStateStorage {..} = do
  banStateMessages <- toHSet bannedMessages
  banStateChats <- toHSet bannedChats
  pure BanState {..}

banStateToStorage :: BanState -> IO BanStateStorage
banStateToStorage BanState {..} = do
  bannedMessages <- fromHSet banStateMessages
  bannedChats <- fromHSet banStateChats
  pure BanStateStorage {..}

data BlocklistStorage = BlocklistStorage
  { spamerBans :: HM.HashMap UserId BanStateStorage
  , spamerUsernames :: HM.HashMap Text UserId
  }
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

instance Semigroup BlocklistStorage where
  a <> b = BlocklistStorage
    { spamerBans = spamerBans a <> spamerBans b
    , spamerUsernames = spamerUsernames a <> spamerUsernames b
    }

instance Monoid BlocklistStorage where
  mempty = newBlocklistStorage

newBlocklistStorage :: BlocklistStorage
newBlocklistStorage = BlocklistStorage
  { spamerBans = HM.empty
  , spamerUsernames = HM.empty
  }

storageToBlocklist :: BlocklistStorage -> IO Blocklist
storageToBlocklist BlocklistStorage {..} = do
  let toBanState (k, v) = do
        banState <- storageToBanState v
        pure (k, banState)
  bansList <- mapM toBanState $ HM.toList spamerBans
  blocklistSpamerBans <- HT.fromList bansList
  blocklistSpamerUsernames <- toHMap spamerUsernames
  pure Blocklist {..}

blocklistToStorage :: Blocklist -> IO BlocklistStorage
blocklistToStorage Blocklist {..} = do
  spamerBans <- (sequenceA . HM.map banStateToStorage) =<< fromHMap blocklistSpamerBans
  spamerUsernames <- fromHMap blocklistSpamerUsernames
  pure BlocklistStorage {..}

alterBlocklist
  :: MonadIO m => TVar Blocklist -> UserInfo -> (Maybe BanState -> Maybe BanState) -> m ()
alterBlocklist cache UserInfo{..} modifier = do
  let alterM Blocklist{..} = do
        HT.alter blocklistSpamerBans modifier userInfoId
        forM_ userInfoUsername \username -> do
          let mnormalUsername = normaliseUsername username
          forM_ mnormalUsername \normalUsername -> do
            HT.insert blocklistSpamerUsernames normalUsername userInfoId
  liftIO $! do
    content <- readTVarIO cache
    alterM content
    atomically $! writeTVar cache content

alterBlocklistM
  :: MonadIO m
  => TVar Blocklist -> UserInfo -> (Maybe BanState -> IO (Maybe BanState)) -> m ()
alterBlocklistM cache UserInfo{..} modifier = do
  let alterM Blocklist{..} = do
        liftIO $ HT.alterM blocklistSpamerBans modifier userInfoId
        forM_ userInfoUsername \username -> do
          let mnormalUsername = normaliseUsername username
          forM_ mnormalUsername \normalUsername -> do
            HT.insert blocklistSpamerUsernames normalUsername userInfoId
  liftIO $! do
    content <- readTVarIO cache
    alterM content
    atomically $! writeTVar cache content

lookupBlocklist :: MonadIO m => TVar Blocklist -> UserId -> m (Maybe BanState)
lookupBlocklist cache = lookupCacheWith cache blocklistSpamerBans

isDebugEnabled :: WithBotState => Bool
isDebugEnabled = let BotState{..} = ?model in debugEnabled botSettings

unlessDebug :: (WithBotState, Monad m) => m () -> m ()
unlessDebug action = unless isDebugEnabled $! action

withDebug :: (WithBotState, Monad m) => m () -> m ()
withDebug action = when isDebugEnabled $! action

withLock :: WithBotState => ClientM a -> IO (Either ClientError a)
withLock action = do
  let BotState{clientEnv, requestLock} = ?model
  takeMVar requestLock
  eResult <- runClientM action clientEnv
  putMVar requestLock ()
  pure eResult

withLockAndRetry
  :: forall m a b. (WithBotState, Show a, MonadIO m, a ~ Response b)
  => ClientM a -> m (Maybe a)
withLockAndRetry action = liftIO $ do
  let BotState{botSettings} = ?model
      Settings{..} = botSettings

      retryActionResponse retries
        | retries <= 1 = do
            log' @String "Too many retries"
            withLock action
        | otherwise = do
            eResult <- withLock action
            case eResult of
              Left err -> case err of
                ConnectionError exc -> case fromException exc of
                  -- Underlying http-client fails to establish connection
                  Just (HttpExceptionRequest _req ConnectionTimeout) -> do
                    log' $ concat
                      [ "connection timeout. ", show retries, " retries left."]
                    wait 1
                    retryActionResponse (pred retries)
                  _ -> log' err >> pure (Left err)
                _ -> log' err >> pure (Left err)
              Right result -> do
                if (responseOk result)
                  then pure (Right result)
                  else do
                    -- we're inspecting something similar to "429 too many requests",
                    -- telegram will tell how long to wait before trying again
                    let mTimeoutSec = responseParameters result
                          >>= responseParametersRetryAfter
                    case mTimeoutSec of
                      Nothing -> pure (Right result)
                      Just timeoutSec -> do
                        log' $ concat
                          [ "Telegram respond us with retry_after "
                          , show timeoutSec
                          , " seconds. ", show retries
                          , " retries left."
                          ]
                        wait $ coerce timeoutSec
                        retryActionResponse (pred retries)

  eResult <- retryActionResponse (fromIntegral @_ @Int botCallRetries)
  let mResult = either (const Nothing) Just eResult
  pure mResult

call :: WithBotState => (MonadIO m, Show a, a ~ Response b) => ClientM a -> m (Maybe a)
call = liftIO . callIO

callIO :: (WithBotState, Show a, a ~ Response b) => ClientM a -> IO (Maybe a)
callIO action = do
  response <- liftIO (withLockAndRetry action)
  waitMs 400
  pure response

callCasCheck :: WithBotState => MonadIO m => UserId -> m (Maybe [Text])
callCasCheck userId = do
  let BotState{..} = ?model
      Settings{..} = botSettings

  if not (casEnabled cas)
    then pure Nothing
    else do
      eResponse <- liftIO $ checkUser casClient cas userId
      case eResponse of
        Left str -> liftIO (log' str) >> pure Nothing
        Right CasResponse{..} -> pure $
          if casResponseOk
            then casResultMessages <$> casResponseResult
            else Nothing  

wait :: Int -> IO ()
wait = threadDelay . (1_000_000 *)

waitMs :: Int -> IO ()
waitMs = threadDelay . (1_000 *)

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
