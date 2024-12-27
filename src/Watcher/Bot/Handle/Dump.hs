module Watcher.Bot.Handle.Dump where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (getCurrentTime)

import Watcher.Bot.Cache
import Watcher.Bot.Settings
import Watcher.Bot.State

dumpAllCachesOnce :: (WithBotState, MonadIO m) => m ()
dumpAllCachesOnce = do
  let BotState {..} = ?model
      Settings{..} = botSettings
      StorageSettings{..} = storage

  liftIO do
    now <- getCurrentTime
    dumpCache now groupsPath groups
    dumpCache now adminsPath admins
    dumpCache now usersPath users
    blocklistCache <- newTVarIO =<< blocklistToStorage blocklist
    dumpCache now blocklistPath blocklistCache
    dumpCache now spamMessagesPath spamMessages
    dumpCache now eventSetPath eventSet

