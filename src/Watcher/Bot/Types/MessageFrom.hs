module Watcher.Bot.Types.MessageFrom where

import Data.Coerce (coerce)
import Data.Maybe (fromMaybe, isJust)
import Telegram.Bot.API

import Watcher.Bot.Settings
import Watcher.Orphans ()

data MessageFrom
  = OwnerGroup
  | DirectMessage UserId
  | PublicGroup ChatId UserId
  | PrivateGroup ChatId UserId
  | Channel ChatId
  | Unsupported ChatId

messageSentFrom :: Settings -> Message -> MessageFrom
messageSentFrom botSettings Message{..} =
  let mOwnerId = fmap ownerGroupId . ownerGroup $ botSettings
      isOwner = Just (coerce $ chatId messageChat) == mOwnerId
      ctype = chatType messageChat
      -- TODO/upstream: missing Eq instance
      isGroup = ctype `elem` [ChatTypeGroup, ChatTypeSupergroup]
      isChannel = ctype == ChatTypeChannel
      cid = fromMaybe (chatId messageChat) messageMigrateToChatId
      title = chatTitle messageChat
      withUser t = case userId <$> messageFrom of
        Nothing -> Unsupported cid
        Just uid -> t uid
  in if isOwner then OwnerGroup else
       if isChannel then Channel cid else
         case (isGroup, isJust title) of
           (True, True) -> withUser (PublicGroup cid)
           (True, False) -> withUser (PrivateGroup cid)
           (False, _) -> withUser DirectMessage
