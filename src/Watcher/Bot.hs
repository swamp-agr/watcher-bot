module Watcher.Bot where

import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Monad (forever, forM, forM_, join, when)
import Data.IORef (readIORef, newIORef, modifyIORef')
import Data.Text (Text)
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

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Options.Applicative as OA

import Watcher.Bot.Cache
import Watcher.Bot.Handle
import Watcher.Bot.Handle.Ban
import Watcher.Bot.Handle.Dump
import Watcher.Bot.Handle.Message
import Watcher.Bot.ModelChecker
import Watcher.Bot.Parse
import Watcher.Bot.Reply
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.State.Chat
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
  mResponse <- callIO model getMe
  let botItself = maybe (error "getMe failed") (userToUserInfo . responseResult) mResponse
  atomically $ modifyTVar' self (const $ Just botItself)


gatherCacheStats :: (a ~ cache content, Foldable cache) => Text -> TVar a -> IO Text
gatherCacheStats title cache = do
  cacheSize <- getCacheSize cache
  pure $ Text.concat
    [ title, ": ", s2t cacheSize]

gatherStatistics :: BotState -> IO ()
gatherStatistics model@BotState{..} = every statistics $ do
  groupsStats <- gatherCacheStats "Groups" groups
  adminsStats <- gatherCacheStats "Admins" admins
  usersStats <- gatherCacheStats "Users" users
  blocklistStats <- gatherCacheStats "Blocklist" blocklist
  spamMessagesStats <- gatherCacheStats "Spam messages" spamMessages
  selfDestructionSetStats <- gatherCacheStats "Self-destruct message queue" selfDestructionSet
  replyStats model $ Text.unlines
    [ "Statistics"
    , ""
    , groupsStats
    , adminsStats
    , usersStats
    , blocklistStats
    , spamMessagesStats
    , selfDestructionSetStats
    ]
  where
    Settings {..} = botSettings
    WorkersSettings {..} = workers

autoban :: BotState -> IO ()
autoban model@BotState{..} = do
  let makeChatReport chatId ref = do
        chatStats <- readIORef ref
        let lineToText (status, count) = Text.concat [ "- ", status, ": ", s2t count ]
            message = Text.concat
              [ s2t chatId, ": \n"
              , if HM.null chatStats
                  then "(empty)"
                  else Text.unlines (lineToText <$> HM.toList chatStats)
              ]
        pure message

      replyMessages msg ref = do
        count <- readIORef ref
        let message = Text.concat
              [ "Quarantine cleanup\n\nTotal: ", s2t count, " chats\n\n"
              , Text.unlines msg
              ]
        replyStats model message

  groupsMap <- readCache groups
  chatCounter <- newIORef (0 :: Int)

  messages <- forM (HM.toList groupsMap) $ \(chatId, ChatState{..}) -> do
    chatMemberStats <- newIORef (HM.empty @Text @Int)
    forM_ (HM.keys quarantine) $ \userId -> do
      mResponse <- callIO model $ getChatMember (SomeChatId chatId) userId

      let status = maybe "unknown" (chatMemberStatus . responseResult) mResponse :: Text
          go Nothing = Just 1
          go (Just v) = Just $! v + 1
      modifyIORef' chatMemberStats (HM.alter go status)

      when (status == "kicked") $ do
        updateBlocklist model chatId userId Nothing
        endQuarantineForUser model chatId userId

    modifyIORef' chatCounter (+ 1)
    makeChatReport chatId chatMemberStats
  replyMessages messages chatCounter

-- | Initiate Telegram Env, 'Model', start Bot, start backends concurrently.
runTelegramBot :: Model -> IO ()
runTelegramBot st@BotState{..} = do
  botActionFun <- startBotAsync (watcherBot st) clientEnv
  runConcurrently $
    Concurrently (selfDestructMessages st botActionFun) <*
    Concurrently (cleanAllCaches st) <*
    Concurrently (dumpAllCaches st) <*
    Concurrently (getSelf st) <*
    Concurrently (gatherStatistics st) <*
    Concurrently (autoban st)

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
