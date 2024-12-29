module Watcher.Bot.Handle.Unban where

import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Time (getCurrentTime)
import Telegram.Bot.API
import Telegram.Bot.Simple

import qualified Data.HashSet as HS

import Watcher.Bot.Analytics
import Watcher.Bot.Cache
import Watcher.Bot.Handle.Ban
import Watcher.Bot.Reply
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.State.Chat
import Watcher.Bot.Types
import Watcher.Bot.Utils

handleUnbanAction
  :: WithBotState
  => ChatId
  -> ChatState
  -> UserId
  -> MessageId
  -> SomeChatId
  -> BotM ()
handleUnbanAction chatId ch adminId messageId someChatId = do
  let BotState {..} = ?model
  begin <- liftIO getCurrentTime
  sendEvent (chatEvent begin chatId EventGroupUnban)
  
  void $ call (deleteMessage chatId messageId)
  when (adminId `HS.member` chatAdmins ch) $ do
    mUserId <- case someChatId of
      SomeChatId userChatId -> pure $ Just $ coerce @_ @UserId userChatId
      SomeChatUsername username ->
        lookupCacheWith blocklist spamerUsernames username >>= \case
          Nothing -> do
            selfDestructReply chatId ch (ReplyUnknownUsername username)
            pure Nothing
          Just userId -> pure (Just userId)
    forM_ mUserId $ \userId -> do
      let userChatId = coerce @_ @ChatId userId
      mResponse <- call (getChat $ SomeChatId userChatId)
      forM_ mResponse $ \chatResponse -> do
        let c = responseResult chatResponse
            userInfo = chatFullInfoToUserInfo c
        lookupBlocklist blocklist userId >>= \case
          Nothing -> selfDestructReply chatId ch (ReplyUserHasNotBeenBanned userInfo)
          Just BanState{..} -> if bannedChats `elem` [HS.singleton chatId, HS.empty]
            then do
              end <- liftIO getCurrentTime
              let evt = (chatEvent end chatId EventGroupUnban)
                    { eventUserId = Just adminId
                    , eventData = Just "unban_globally"
                    }
              sendEvent evt

              alterBlocklist blocklist userInfo $! const Nothing
              let unbanReq = defUnbanChatMember (SomeChatId chatId) userId
              mUnbanResponse <- call $ unbanChatMember unbanReq
              when ((responseResult <$> mUnbanResponse) == Just True) $ 
                selfDestructReply chatId ch (ReplyUserHasBeenUnbanned userInfo)
            else do
              end <- liftIO getCurrentTime
              let evt = (chatEvent end chatId EventGroupUnban)
                    { eventUserId = Just adminId
                    , eventData = Just "unban_locally"
                    }
              sendEvent evt

              allowUserInGroup ch chatId userId
              let unbanReq = defUnbanChatMember (SomeChatId chatId) userId
              mUnbanResponse <- call $ unbanChatMember unbanReq
              when ((fmap responseResult mUnbanResponse) == Just True) $ 
                selfDestructReply chatId ch (ReplyUserHasBeenUnbanned userInfo)

-- | This could be originated in owners group only.
handleGlobalUnbanAction :: WithBotState => MessageId -> SomeChatId -> BotM ()
handleGlobalUnbanAction messageId someChatId = do
  let BotState {..} = ?model
      Settings {..} = botSettings
  forM_ ownerGroup $ \OwnerGroupSettings {..} -> do
    let chatId = ChatId ownerGroupId
    begin <- liftIO getCurrentTime
    sendEvent (chatEvent begin chatId EventGroupUnban)

    void $ call (deleteMessage chatId messageId)
    mResponse <- call (getChat someChatId)
    forM_ mResponse $ \chatResponse -> do
      let c = responseResult chatResponse
          userInfo = chatFullInfoToUserInfo c
          userId = userInfoId userInfo
      lookupBlocklist blocklist userId >>= \case
        Nothing -> replyText $ "user not banned: " <> s2t userInfo
        Just BanState {..} -> do
          alterBlocklist blocklist userInfo $! const Nothing

          forM_ bannedChats $ \banChatId -> do
            let unbanReq = defUnbanChatMember (SomeChatId banChatId) userId
            void $ call $ unbanChatMember unbanReq
