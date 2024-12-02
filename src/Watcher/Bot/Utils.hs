module Watcher.Bot.Utils where

import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

s2t :: Show a => a -> Text
s2t = Text.pack . show

log' :: Show a => a -> IO ()
log' x = do
  now <- getCurrentTime
  putStrLn $ show now <> ": " <> show x

logT :: Text -> IO ()
logT txt = do
  now <- getCurrentTime
  Text.putStrLn $ s2t now <> ": " <> txt

notLongAgoEnough :: UTCTime -> UTCTime -> Bool
notLongAgoEnough prev next = diffUTCTime next prev > 86400.0
