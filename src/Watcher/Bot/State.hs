module Watcher.Bot.State where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, modifyTVar')
import Control.Exception (fromException)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime (..))
import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( Manager, HttpException(..), HttpExceptionContent(..)
  , managerResponseTimeout, newManager, responseTimeoutMicro
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Katip
import Servant.Client (ClientEnv, ClientError (..), ClientM, mkClientEnv, runClientM)
import System.IO (stdout)
import Telegram.Bot.API
import Telegram.Bot.API.Names

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
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
  , blocklist :: TVar Blocklist
  , spamMessages :: TVar SpamMessages
  , eventSet :: TVar Events
  }

-- | Bot has its own state
newBotState :: Settings -> IO BotState
newBotState settings@Settings{..} = do
  logEnv <- makeLogEnv
  admins <- newTVarIO HM.empty
  clientEnv <- createTelegramClientEnv (Token botToken) (fromIntegral botResponseTimeout)
  groups <- newTVarIO HM.empty
  users <- newTVarIO HM.empty
  blocklist <- newTVarIO newBlocklist
  spamMessages <- newTVarIO HM.empty
  requestLock <- newMVar ()
  eventSet <- newTVarIO Set.empty
  let adultEmoji = Text.foldl' (flip HS.insert) HS.empty . scoreAdultEmoji $! scores
  self <- newTVarIO Nothing
  casClient <- newCasClient
  pure BotState { botSettings = settings, .. }

importBotState :: Settings -> IO BotState
importBotState settings@Settings {..} = do
  let StorageSettings {..} = storage
      adultEmoji = Text.foldl' (flip HS.insert) HS.empty . scoreAdultEmoji $! scores

  clientEnv <- createTelegramClientEnv (Token botToken) (fromIntegral botResponseTimeout)
  requestLock <- newMVar ()
  self <- newTVarIO Nothing
  logEnv <- makeLogEnv
  casClient <- newCasClient

  admins <- importCache adminsPath
  groups <- importCache groupsPath
  users <- importCache usersPath
  blocklist <- importCache blocklistPath
  spamMessages <- importCache spamMessagesPath
  eventSet <- importCache eventSetPath

  pure $ BotState { botSettings = settings, .. }

createTelegramClientEnv :: Token -> Int -> IO ClientEnv
createTelegramClientEnv token timeoutSec =
  let respTimeout = responseTimeoutMicro (timeoutSec * 1_000_000)
  in mkClientEnv
     <$> newManager (tlsManagerSettings { managerResponseTimeout = respTimeout })
     <*> pure (botBaseUrl token)

data BanState = BanState
  { bannedMessages :: HashSet MessageText
  , bannedChats :: HashSet ChatId
  } deriving (Eq, Show, Generic, FromDhall, ToDhall)

instance Semigroup BanState where
  a <> b = BanState
    { bannedMessages = HS.union (bannedMessages a) (bannedMessages b)
    , bannedChats = HS.union (bannedChats a) (bannedChats b)
    }

newBanState :: BanState
newBanState = BanState
  { bannedMessages = HS.empty
  , bannedChats = HS.empty
  }

data Blocklist = Blocklist
  { spamerBans :: HashMap UserId BanState
  , spamerUsernames :: HashMap Text UserId
  }
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

instance Semigroup Blocklist where
  a <> b = Blocklist
    { spamerBans = spamerBans a <> spamerBans b
    , spamerUsernames = spamerUsernames a <> spamerUsernames b
    }

instance Monoid Blocklist where
  mempty = newBlocklist

newBlocklist :: Blocklist
newBlocklist = Blocklist
  { spamerBans = HM.empty
  , spamerUsernames = HM.empty
  }

alterBlocklist
  :: MonadIO m => TVar Blocklist -> UserInfo -> (Maybe BanState -> Maybe BanState) -> m ()
alterBlocklist cache UserInfo{..} modifier =
  let modifyUsername x = case userInfoUsername of
        Nothing -> x
        Just username ->
          let normalUsername = normaliseUsername username
              insertUsername u = HM.insert u userInfoId x
          in maybe x insertUsername normalUsername
      alter b@Blocklist{..} = b
        { spamerBans = HM.alter modifier userInfoId spamerBans
        , spamerUsernames = modifyUsername spamerUsernames
        }
  in liftIO $! atomically $! modifyTVar' cache $! alter

lookupBlocklist :: MonadIO m => TVar Blocklist -> UserId -> m (Maybe BanState)
lookupBlocklist cache = lookupCacheWith cache spamerBans

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
