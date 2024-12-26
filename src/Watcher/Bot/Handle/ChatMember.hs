module Watcher.Bot.Handle.ChatMember where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust)
import Data.Text (Text)
import Telegram.Bot.API

import Watcher.Bot.Handle.Ban
import Watcher.Bot.Handle.Message
import Watcher.Bot.State
import Watcher.Bot.Types

handleCheckChatMember
  :: (WithBotState, ToUserInfo user, MonadIO m) => ChatId -> user -> m Text
handleCheckChatMember chatId user = do
  let userId = toUserId user
  mResponse <- call $ getChatMember (SomeChatId chatId) userId
  mMessages <- callCasCheck userId

  let status = maybe "unknown" (chatMemberStatus . responseResult) mResponse :: Text
  when (status == "kicked" || isJust mMessages) $ do
    let texts = maybe [] (fmap MessageText) mMessages
    updateBlocklist chatId (toUserInfo user) texts
    endQuarantineForUser chatId userId

  pure status
