module Watcher.Bot.Types.MessageInfo where

import Data.Text (Text)
import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)
import Telegram.Bot.API

import Watcher.Bot.Types.ChatInfo
import Watcher.Bot.Types.UserInfo

  
data MessageInfo = MessageInfo
  { messageInfoId :: MessageId
  , messageInfoThreadId :: Maybe MessageThreadId
  , messageInfoText :: Maybe Text
  , messageInfoFrom :: Maybe UserInfo
  , messageInfoChat :: ChatInfo
  } deriving (Show, Eq, Generic, FromDhall, ToDhall)

messageToMessageInfo :: Message -> MessageInfo
messageToMessageInfo Message{..} = MessageInfo
  { messageInfoId = messageMessageId
  , messageInfoThreadId = messageMessageThreadId
  , messageInfoText = messageText
  , messageInfoFrom = userToUserInfo <$> messageFrom
  , messageInfoChat = chatToChatInfo messageChat
  }
