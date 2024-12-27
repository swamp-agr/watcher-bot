{-# LANGUAGE AllowAmbiguousTypes #-}
module Watcher.Migration where

import Control.Exception (SomeException, evaluate, try)
import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe, listToMaybe)
import Dhall
import System.Directory (listDirectory)
import System.FilePath ((</>), (<.>), isExtensionOf, takeBaseName, takeDirectory)
import Text.Read (readMaybe)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Watcher.Bot.Cache
import Watcher.Bot.Settings
import Watcher.Bot.State

findRecentMigration :: FilePath -> IO FilePath
findRecentMigration dir = do
  let migrationNumber file = (, file) $ readMaybe @Int $ takeWhile isDigit file
  contents <- filter (isExtensionOf ".dhall") <$> listDirectory dir
  pure
    . (dir </>)
    . fromMaybe (error "No migrations found")
    . listToMaybe
    . sortOn (Down . migrationNumber)
    $ contents

nextCachePath :: FilePath -> FilePath
nextCachePath cachePath =
  let dir = takeDirectory cachePath
      base = takeBaseName cachePath
      next = succ $ fromMaybe 0 $ readMaybe @Int (takeWhile isDigit base)
      remainder = dropWhile isDigit base
  in dir </> (show next <> remainder) <.> "dhall"

migrate :: IO ()
migrate = do
  Settings {..} <- loadDefaultSettings
  let StorageSettings {..} = storage

  recentMigrationPath <- findRecentMigration "./config/migrations"
  putStrLn $ "The most recent migration: " <> recentMigrationPath

  let run
        :: forall (a :: Type) . (FromDhall a, ToDhall a)
        => (Text, FilePath) -> IO ()
      run = runMigration @a recentMigrationPath

  run @Admins ("migrateAdmins", adminsPath)
  run @BlocklistStorage ("migrateBlocklist", blocklistPath)
  run @Groups ("migrateGroups", groupsPath)
  run @SpamMessages ("migrateSpamMessages", spamMessagesPath)
  run @Users ("migrateUsers", usersPath)
  run @Events ("migrateEvents", eventSetPath)

runMigration
  :: forall (a :: Type). (FromDhall a, ToDhall a)
  => FilePath -> ( Text, FilePath) -> IO ()
runMigration migrationPath (migrateFun, cachePathPiece) = do
  let getRecentCachePath path = do
        mCachePath <- getRecentCacheFilePathMaybe path
        pure $ fromMaybe (error $ "cache not found: " <> cachePathPiece) mCachePath
  eAdmins <- try $ do
    putStr $ "Checking " <> cachePathPiece
    cachePath <- getRecentCachePath cachePathPiece
    load @a (Text.pack cachePath) >>= evaluate
  case eAdmins of
    Right _ -> putStr $ "\tOK\n"
    Left (_err :: SomeException) -> do
      putStr $ "\tMigrating...\n"
      cachePath <- getRecentCachePath cachePathPiece
      putStrLn $ "The most recent cache: " <> cachePath
      let migratedCachePath = nextCachePath cachePath
      applyMigration @a migrateFun migrationPath cachePath migratedCachePath
      putStrLn $ "Output has been written to: " <> migratedCachePath

applyMigration
  :: forall a. (FromDhall a, ToDhall a)
  => Text -> FilePath -> FilePath -> FilePath -> IO ()
applyMigration fun migration cachePath outputCachePath = do
  let expr = Text.concat
        [ "let cache = ", Text.pack cachePath
        , " in (", Text.pack migration, ").", fun, " cache"
        ]
  content <- evaluate =<< load @a expr
  let txt = renderPretty content
  Text.writeFile outputCachePath txt
  putStrLn "Migration applied: OK"
