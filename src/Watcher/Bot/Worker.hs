module Watcher.Bot.Worker where

import Control.Monad (forever, forM_, unless)
import GHC.Stack (HasCallStack)
import Data.Time
  (LocalTime (..), daysAndTimeOfDayToTime, getCurrentTime, utc, utcToLocalTime)

import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.Utils

every :: WithBotState => HasCallStack => WorkerSettings -> IO () -> IO ()
every WorkerSettings{..} action = do
  logT $ "Start worker: " <> workerName
  forever $ do
    now <- getCurrentTime
    let todToSec = daysAndTimeOfDayToTime 0
        todNow = localTimeOfDay (utcToLocalTime utc now)
        getDelta a b = if a <= b
          then todToSec b - todToSec a
          else 86400 - (todToSec a - todToSec b)
    forM_ workerMode \case
      WorkerRange{..} -> do
        let delta = getDelta todNow workerRangeFrom
        unless (workerRangeFrom <= todNow && todNow <= workerRangeTo) do
          logT $ "Too early: " <> workerName <> " is waiting... " <> s2t delta <> " seconds"
          wait (round delta)

      WorkerAt{..} -> do
        let delta = getDelta todNow workerAt
        logT $ "Too early: " <> workerName <> " is waiting... " <> s2t delta <> " seconds"
        wait (round delta)

    logT $ "Run worker: " <> workerName
    action
    wait (fromIntegral workerPeriodUnits * workerPeriodToSec workerPeriod)
