module Watcher.Bot.Cache where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar, newTVarIO)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, join, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Time (UTCTime (..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Dhall (FromDhall (..), ToDhall (..), embed, inject)
import Dhall.Pretty (prettyExpr)
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, getFileSize, listDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>), (<.>))

import qualified Data.Foldable as Fold
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Watcher.Bot.Settings

getAbsoluteCachePath :: FilePath -> IO FilePath
getAbsoluteCachePath dir = do
  current <- (round @_ @Int . realToFrac @_ @Double) <$> getPOSIXTime
  let newFilePath = dir </> (show current) <.> "dhall"
  pure newFilePath

getLastFilepath :: FilePath -> IO (Maybe FilePath)
getLastFilepath d = doesDirectoryExist d >>= \case
  False -> pure Nothing
  True -> fmap (listToMaybe . sortOn Down) . listDirectory $ d

getOrCreateCacheDir :: UTCTime -> FilePath -> IO FilePath
getOrCreateCacheDir time cache = do
  let UTCTime day _ = time
      dayDir = iso8601Show day
      path = "cache" </> dayDir </> cache
  createDirectoryIfMissing True path
  pure path

dumpCache :: (ToDhall a, Show a) => UTCTime -> FilePath ->  TVar a -> IO ()
dumpCache time dir cache = do
  absoluteDir <- getOrCreateCacheDir time dir
  cachePath <- getAbsoluteCachePath absoluteDir
  cacheContent <- evaluate =<< (atomically $! readTVar cache)
  let txt = renderPretty cacheContent
  Text.writeFile cachePath txt

getCacheSize :: Foldable cache => TVar (cache content) -> IO Int
getCacheSize cache = do
  content <- atomically $! readTVar cache
  pure $! Fold.length content

importCache :: (FromDhall a, Monoid a) => FilePath -> IO (TVar a)
importCache dir = do
  mDayDir <- getLastFilepath "cache"
  mCache <- forM mDayDir $ \dayDir -> do
    mFilePath <- getLastFilepath ("cache" </> dayDir </> dir)
    forM mFilePath $ \file -> do
      let fullPath = "." </> "cache" </> dayDir </> dir </> file
      load (Text.pack fullPath)
  newTVarIO (fromMaybe mempty $ join mCache)

cleanCache :: FilePath -> IO ()
cleanCache dir = do
  mDayDir <- getLastFilepath "cache"
  forM_ mDayDir $ \dayDir -> do
    mFilePath <- getLastFilepath ("cache" </> dayDir </> dir)
    forM mFilePath $ \file -> do
      let fullPath = "." </> "cache" </> dayDir </> dir </> file
      filesize <- getFileSize fullPath
      when (filesize > 0) $ do
        days <- filter (/= dayDir) <$> listDirectory "cache"
        let makeCacheDir d = "cache" </> d </> dir
        forM_ days $ \day -> removeDirectoryIfExist (makeCacheDir day)

lookupCache :: (MonadIO m, Hashable k) => TVar (HashMap k v) -> k -> m (Maybe v)
lookupCache cache key = liftIO $! atomically $! do
  readTVar cache >>= \map' -> pure $! HM.lookup key map'

writeCache :: (MonadIO m, Hashable k) => TVar (HashMap k v) -> k -> v -> m ()
writeCache cache key value =
  liftIO $! atomically $! modifyTVar' cache $! HM.insert key value 

alterCache
  :: (MonadIO m, Hashable k)
  => TVar (HashMap k v) -> k -> (Maybe v -> Maybe v) -> m ()
alterCache cache key modifier =
  liftIO $! atomically $! modifyTVar' cache $! HM.alter modifier key


removeDirectoryIfExist :: FilePath -> IO ()
removeDirectoryIfExist dir = do
  exist <- doesDirectoryExist dir
  when exist $ removeDirectoryRecursive dir

renderPretty :: ToDhall a => a -> Text
renderPretty = Text.pack . show . prettyExpr . embed inject

