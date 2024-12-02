module Watcher.Bot.Types.ChatInfo where

import Data.Text (Text)
import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)
import Telegram.Bot.API
import Telegram.Bot.API.Names

import Watcher.Orphans ()

data ChatInfo = ChatInfo
  { chatInfoId :: ChatId
  , chatInfoName :: Text
  , chatInfoTitle :: Maybe Text
  , chatInfoBio :: Maybe Text
  , chatInfoEmojiStatusCustomEmojiId :: Maybe Text
  , chatInfoBackgroundCustomEmojiId :: Maybe Text
  } deriving (Show, Eq, Generic, FromDhall, ToDhall)

chatToChatInfo :: Chat -> ChatInfo
chatToChatInfo c@Chat{..} = ChatInfo
  { chatInfoId = chatId
  , chatInfoName = getChatName c
  , chatInfoTitle = chatTitle
  , chatInfoBio = Nothing
  , chatInfoEmojiStatusCustomEmojiId = Nothing
  , chatInfoBackgroundCustomEmojiId = Nothing
  }

toChatInfo :: ChatFullInfo -> ChatInfo
toChatInfo c@ChatFullInfo{..} = ChatInfo
  { chatInfoId = chatFullInfoId
  , chatInfoName = getChatFullInfoName c
  , chatInfoTitle = chatFullInfoTitle
  , chatInfoBio = chatFullInfoBio
  , chatInfoEmojiStatusCustomEmojiId = chatFullInfoEmojiStatusCustomEmojiId
  , chatInfoBackgroundCustomEmojiId = chatFullInfoBackgroundCustomEmojiId
  }
