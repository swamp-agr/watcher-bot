module Watcher.Bot.Handle.Dump where

import Data.Time (getCurrentTime)

import Watcher.Bot.Cache
import Watcher.Bot.Settings
import Watcher.Bot.State

dumpAllCachesOnce :: BotState -> IO ()
dumpAllCachesOnce BotState{..} = do
  let Settings{..} = botSettings
      StorageSettings{..} = storage

  now <- getCurrentTime
  dumpCache now groupsPath groups
  dumpCache now adminsPath admins
  dumpCache now usersPath users
  dumpCache now blocklistPath blocklist
  dumpCache now spamMessagesPath spamMessages
  dumpCache now selfDestructionSetPath selfDestructionSet
