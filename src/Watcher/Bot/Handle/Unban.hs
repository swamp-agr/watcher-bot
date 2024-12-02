module Watcher.Bot.Handle.Unban where

import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Telegram.Bot.API
import Telegram.Bot.Simple

import qualified Data.HashSet as HS

import Watcher.Bot.Analytics
import Watcher.Bot.Cache
import Watcher.Bot.Handle.Ban
import Watcher.Bot.Reply
import Watcher.Bot.State
import Watcher.Bot.State.Chat
import Watcher.Bot.Types

handleUnbanAction
  :: BotState
  -> ChatId
  -> ChatState
  -> UserId
  -> MessageId
  -> SomeChatId
  -> BotM ()
handleUnbanAction model@BotState{..} chatId ch adminId messageId someChatId = do
  begin <- liftIO getCurrentTime
  sendEvent model (chatEvent begin chatId EventGroupUnban)
  
  void $ call model (deleteMessage chatId messageId)
  when (adminId `HS.member` chatAdmins ch) $ do
    mResponse <- call model (getChat someChatId)
    forM_ mResponse $ \chatResponse -> do
      let c = responseResult chatResponse
          userInfo = chatFullInfoToUserInfo c
          userId = userInfoId userInfo
      lookupCache blocklist userId >>= \case
        Nothing -> selfDestructReply model chatId (ReplyUserHasNotBeenBanned userInfo)
        Just BanState{..} -> if HS.singleton chatId == bannedChats
          then do
            end <- liftIO getCurrentTime
            let evt = (chatEvent end chatId EventGroupUnban)
                  { eventUserId = Just adminId
                  , eventData = Just "unban_globally"
                  }
            sendEvent model evt
            
            alterCache blocklist userId $! const Nothing
            let unbanReq = defUnbanChatMember (SomeChatId chatId) userId
            mUnbanResponse <- call model $ unbanChatMember unbanReq
            when ((responseResult <$> mUnbanResponse) == Just True) $ 
              selfDestructReply model chatId (ReplyUserHasBeenUnbanned userInfo)
          else do
            end <- liftIO getCurrentTime
            let evt = (chatEvent end chatId EventGroupUnban)
                  { eventUserId = Just adminId
                  , eventData = Just "unban_locally"
                  }
            sendEvent model evt

            allowUserInGroup model ch chatId userId
            let unbanReq = defUnbanChatMember (SomeChatId chatId) userId
            mUnbanResponse <- call model $ unbanChatMember unbanReq
            when ((fmap responseResult mUnbanResponse) == Just True) $ 
              selfDestructReply model chatId (ReplyUserHasBeenUnbanned userInfo)
