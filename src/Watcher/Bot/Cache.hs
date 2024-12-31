module Watcher.Bot.Cache where

import Codec.Compression.Zstd (compress, maxCLevel)
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
import Data.Time.Clock.POSIX (getCurrentTime, getPOSIXTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Dhall (FromDhall (..), ToDhall (..), embed, inject)
import Dhall.Pretty (prettyExpr)
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, getFileSize, listDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>), (<.>))

import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString as BS
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

getRecentCacheFilePathMaybe :: FilePath -> IO (Maybe FilePath)
getRecentCacheFilePathMaybe dir = do
  mDayDir <- getLastFilepath "cache"
  let getFullPathMaybe dayDir = do
        mFilePath <- getLastFilepath ("cache" </> dayDir </> dir)
        let makeFullPath :: FilePath -> FilePath
            makeFullPath file = "." </> "cache" </> dayDir </> dir </> file
        pure (makeFullPath <$> mFilePath)
  maybe (pure Nothing) getFullPathMaybe mDayDir

importCache :: (FromDhall a, Monoid a) => FilePath -> IO (TVar a)
importCache dir = do
  mDayDir <- getLastFilepath "cache"
  mCache <- forM mDayDir $ \dayDir -> do
    mFilePath <- getLastFilepath ("cache" </> dayDir </> dir)
    forM mFilePath $ \file -> do
      let fullPath = "." </> "cache" </> dayDir </> dir </> file
      load (Text.pack fullPath)
  newTVarIO (fromMaybe mempty $ join mCache)

compareTwoCaches :: FilePath -> IO (Maybe (Integer, Integer))
compareTwoCaches dir = do
  mDayDir <- getLastFilepath "cache"
  files <- forM mDayDir $ \dayDir -> do
    let cacheDir = "cache" </> dayDir </> dir
    doesDirectoryExist cacheDir >>= \case
      False -> pure []
      True -> (fmap (cacheDir </>) . take 2 . sortOn Down) <$> listDirectory cacheDir
  case fromMaybe [] files of
    [] -> pure Nothing
    x : [] -> do
      fs <- getFileSize x
      pure $ Just (0, fs)
    x : y : _ -> do
      xs <- getFileSize x
      ys <- getFileSize y
      pure $ Just (ys, xs)

archiveCache :: FilePath -> IO FilePath
archiveCache archiveDir = do
  createDirectoryIfMissing True archiveDir

  days <- (take 5 . sortOn Down) <$> listDirectory "cache"
  paths <- forM days \day -> do
    caches <- listDirectory ("cache" </> day)
    forM caches \cache -> do
      files <- listDirectory ("cache" </> day </> cache)
      forM files \file -> pure ("cache" </> day </> cache </> file)

  day <- utctDay <$> getCurrentTime
  let tarPath = archiveDir </> iso8601Show day <.> "tar"
      archivePath = tarPath <.> "zst"
      tarToZst = compress maxCLevel
  Tar.create tarPath "." (concat $ concat paths)
  BS.readFile tarPath >>= BS.writeFile archivePath . tarToZst
  pure archivePath

cleanCache :: FilePath -> IO ()
cleanCache dir = do
  mDayDir <- getLastFilepath "cache"
  forM_ mDayDir $ \dayDir -> do
    mFilePath <- getLastFilepath ("cache" </> dayDir </> dir)
    forM mFilePath $ \file -> do
      let fullPath = "." </> "cache" </> dayDir </> dir </> file
      filesize <- getFileSize fullPath
      when (filesize > 0) $ do
        -- keep last five days of cache
        days <- drop 5 . sortOn Down <$> listDirectory "cache"
        let makeCacheDir d = "cache" </> d </> dir
        forM_ days $ \day -> removeDirectoryIfExist (makeCacheDir day)

lookupCacheWith
  :: (MonadIO m, Hashable k) => TVar cache -> (cache -> HashMap k v) -> k -> m (Maybe v)
lookupCacheWith cache mapFromCache key = liftIO $! atomically $! do
  readTVar cache >>= \content -> pure $! HM.lookup key $! mapFromCache content

lookupCache :: (MonadIO m, Hashable k) => TVar (HashMap k v) -> k -> m (Maybe v)
lookupCache cache key = lookupCacheWith cache id key

writeCache :: (MonadIO m, Hashable k) => TVar (HashMap k v) -> k -> v -> m ()
writeCache cache key value =
  liftIO $! atomically $! modifyTVar' cache $! HM.insert key value 

alterCache
  :: (MonadIO m, Hashable k)
  => TVar (HashMap k v) -> k -> (Maybe v -> Maybe v) -> m ()
alterCache cache key modifier =
  liftIO $! atomically $! modifyTVar' cache $! HM.alter modifier key

readCache
  :: MonadIO m => TVar cache -> m cache
readCache = liftIO . atomically . readTVar

readCacheWith
  :: (MonadIO m, Hashable k) => (cache -> HashMap k v) -> TVar cache -> m (HashMap k v)
readCacheWith getter cache = (liftIO . atomically . readTVar) cache >>= pure . getter

removeDirectoryIfExist :: FilePath -> IO ()
removeDirectoryIfExist dir = do
  exist <- doesDirectoryExist dir
  when exist $ removeDirectoryRecursive dir

renderPretty :: ToDhall a => a -> Text
renderPretty = Text.pack . show . prettyExpr . embed inject

