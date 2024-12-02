module Watcher.Bot where

import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Monad (forever, join)
import GHC.Stack (HasCallStack)
import Options.Applicative
  ( auto, execParser, help, helper, info, fullDesc, metavar, long, progDesc
  , option, optional, short
  , strOption, subparser, (<**>)
  )
import Servant.Client (ClientEnv)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.BotApp.Internal 

import qualified Options.Applicative as OA

import Watcher.Bot.Cache
import Watcher.Bot.Handle
import Watcher.Bot.Handle.Dump
import Watcher.Bot.ModelChecker
import Watcher.Bot.Parse
import Watcher.Bot.Reply
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.Types
import Watcher.Bot.Utils
import Watcher.Orphans ()

-- | Initiate bot app based on a 'Model'.
watcherBot :: Model -> BotApp Model Action
watcherBot st = BotApp
  { botInitialModel = st
  , botAction = actionParser
  , botHandler = handleAction
  , botJobs = []
  }
  where
    actionParser update BotState{..} = updateToAction botSettings update

dumpAllCaches :: BotState -> IO ()
dumpAllCaches model@BotState{..} = do
  let Settings {..} = botSettings
      WorkersSettings {..} = workers

  every dump $ dumpAllCachesOnce model

cleanAllCaches :: BotState -> IO ()
cleanAllCaches BotState{..} = do
  let Settings {..} = botSettings
      StorageSettings {..} = storage
      WorkersSettings {..} = workers

  every cleanup $ mapM_ cleanCache
    [ groupsPath, adminsPath, usersPath, blocklistPath, spamMessagesPath, selfDestructionSetPath ]

every :: HasCallStack => WorkerSettings -> IO () -> IO ()
every WorkerSettings{..} action = do
  logT $ "Start worker: " <> workerName
  forever $ do
    logT $ "Run worker: " <> workerName
    action
    wait (fromIntegral workerPeriodUnits * workerPeriodToSec workerPeriod)

getSelf :: BotState -> IO ()
getSelf model@BotState{..} = do
  mResponse <- withLock model getMe
  wait 1
  let botItself = maybe (error "getMe failed") (userToUserInfo . responseResult) mResponse
  atomically $ modifyTVar' self (const $ Just botItself)


-- | Initiate Telegram Env, 'Model', start Bot, start backends concurrently.
runTelegramBot :: Model -> IO ()
runTelegramBot st@BotState{..} = do
  botActionFun <- startBotAsync (watcherBot st) clientEnv
  runConcurrently $
    Concurrently (selfDestructMessages st botActionFun) <*
    Concurrently (cleanAllCaches st) <*
    Concurrently (dumpAllCaches st) <*
    Concurrently (getSelf st)

-- | Copy from 'Telegram.Bot.Simple.BotApp'.
startBotEnv :: BotApp model action -> ClientEnv -> IO (BotEnv model action)
startBotEnv bot env = do
  botEnv <- defaultBotEnv bot env
  _jobThreadIds <- scheduleBotJobs botEnv (botJobs bot)
  _actionsThreadId <- processActionsIndefinitely bot botEnv
  return botEnv

-- | Main function.
run :: IO ()
run = join $ execParser (info (opts <**> helper) fullDesc)

opts :: OA.Parser (IO ())
opts = subparser
  (  OA.command "bot" (info (pure runBot) (progDesc "Run telegram-bot"))
  <> OA.command "modelchecker" (info modelChecker (progDesc "Run model-checker")) 
  )

runBot :: IO ()
runBot = runTelegramBot =<< importBotState =<< loadDefaultSettings

modelChecker :: OA.Parser (IO ())
modelChecker = processChatExport
  <$> strOption
    ( long "input"
    <> short 'i'
    <> metavar "FILE"
    <> help "Input JSON file exported from Telegram Desktop"
    )
  <*> strOption
    ( long "output"
    <> short 'o'
    <> metavar "FILE"
    <> help "Output CSV file"
    )
  <*> optional (option auto
    ( long "from"
    <> short 'f'
    <> metavar "MESSAGE_ID"
    <> help "Resume processing from specific message id"
    ))
