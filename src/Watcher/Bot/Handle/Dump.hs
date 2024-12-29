module Watcher.Bot.Handle.Dump where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time (getCurrentTime)

import qualified Data.Text as Text

import Watcher.Bot.Cache
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.Utils

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
    dumpCache now blocklistPath blocklist
    dumpCache now spamMessagesPath spamMessages
    dumpCache now eventSetPath eventSet


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
