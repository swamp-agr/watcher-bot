module Watcher.Bot.Trigger where

import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Control.Monad (forever)
import Data.Time (diffUTCTime, getCurrentTime)

import qualified Data.Set as Set

import Watcher.Bot.State
import Watcher.Bot.Types

processTriggeredEvents :: WithBotState => (Action -> IO ()) -> IO ()
processTriggeredEvents fun = forever $! do
  let BotState{..} = ?model
  queue <- atomically $ readTVar eventSet
  case Set.lookupMin queue of
    Nothing -> wait 1
    Just evt -> do
      atomically $! modifyTVar' eventSet $! Set.delete evt
      now <- getCurrentTime
      case evt of
        SelfDestructMessageEvent SelfDestructMessage{..} ->
          if selfDestructMessageTime < now
            then fun (DeleteMessage selfDestructMessageChatId selfDestructMessageId)
            else do
              let sec = diffUTCTime now selfDestructMessageTime
              wait $ round sec
              fun (DeleteMessage selfDestructMessageChatId selfDestructMessageId)
        UserChatMemberCheckEvent UserChatMemberCheck{..} -> do
          let action = CheckChatMember userChatMemberCheckChatId userChatMemberCheckUserInfo
          if userChatMemberCheckTime < now
            then fun action
            else do
             let sec = diffUTCTime now userChatMemberCheckTime
             wait $ round sec
             fun action
