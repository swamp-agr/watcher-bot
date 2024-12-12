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
messageSentFrom botSettings Message{..}
  = sentFrom botSettings messageChat messageFrom

chatMemberSentFrom :: Settings -> ChatMemberUpdated -> MessageFrom
chatMemberSentFrom botSettings ChatMemberUpdated{..}
  = sentFrom botSettings chatMemberUpdatedChat (Just chatMemberUpdatedFrom)

sentFrom :: Settings -> Chat -> Maybe User -> MessageFrom
sentFrom botSettings fromChat fromUser =
  let mOwnerId = fmap ownerGroupId . ownerGroup $ botSettings
      isOwner = Just (coerce $ chatId fromChat) == mOwnerId
      ctype = chatType fromChat
      isGroup = ctype `elem` [ChatTypeGroup, ChatTypeSupergroup]
      isChannel = ctype == ChatTypeChannel
      cid = chatId fromChat
      username = chatUsername fromChat
      withUser t = case userId <$> fromUser of
        Nothing -> Unsupported cid
        Just uid -> t uid
  in if isOwner then OwnerGroup else
       if isChannel then Channel cid else
         case (isGroup, isJust username) of
           (True, True) -> withUser (PublicGroup cid)
           (True, False) -> withUser (PrivateGroup cid)
           (False, _) -> withUser DirectMessage

data SetupMessageId
  = CallbackSetup MessageId
  | ReplySetup MessageId
  deriving (Eq, Show)

setupToMessageId :: SetupMessageId -> MessageId
setupToMessageId = \case
  CallbackSetup msgId -> msgId
  ReplySetup msgId -> msgId
