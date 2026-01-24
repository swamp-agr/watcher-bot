module Watcher.Bot.Handle.Dump where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time (getCurrentTime)

import qualified Data.Text as Text

import Watcher.Bot.Cache
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.Types
import Watcher.Bot.Utils

dumpAllCachesOnce :: (WithBotState, MonadIO m) => m ()
dumpAllCachesOnce = do
  let BotState {..} = ?model
      Settings{..} = botSettings
      StorageSettings{..} = storage

  liftIO do
    now <- getCurrentTime
    dumpCache now groupsPath groups transformChatExports
    dumpCache now adminsPath admins transformAdminExports
    dumpCache now usersPath users fromHMap
    dumpCache now blocklistPath blocklist blocklistToStorage
    dumpCache now spamMessagesPath spamMessages fromHMap
    dumpCache now eventSetPath eventSet pure


compareDumps :: (WithBotState, MonadIO m) => m Text
compareDumps = liftIO do
  let BotState {..} = ?model
      Settings{..} = botSettings
      StorageSettings{..} = storage

  let getComparisonResult path  =
        liftIO (compareTwoCaches path) >>= \case
          Nothing -> pure $ Just $ "No cache found for: " <> Text.pack path
          Just (before, after)
            | (after < before) -> pure $ Just $
              Text.unlines
                [ "Cache size dropped for cache: " <> Text.pack path
                , "- Before: " <> s2t before
                , "- After: " <> s2t after
                , ""
                ]
            | otherwise -> pure Nothing

  results <- mapM getComparisonResult
    [ groupsPath, adminsPath, usersPath, blocklistPath, spamMessagesPath, eventSetPath ]

  pure $ Text.unlines $ catMaybes results
