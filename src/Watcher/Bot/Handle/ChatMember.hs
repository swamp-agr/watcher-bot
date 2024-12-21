module Watcher.Bot.Handle.ChatMember where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Telegram.Bot.API

import Watcher.Bot.State
import Watcher.Bot.Handle.Ban
import Watcher.Bot.Handle.Message

handleCheckChatMember :: (WithBotState, MonadIO m) => ChatId -> UserId -> m Text
handleCheckChatMember chatId userId = do
  mResponse <- call $ getChatMember (SomeChatId chatId) userId

  let status = maybe "unknown" (chatMemberStatus . responseResult) mResponse :: Text
  when (status == "kicked") $ do
    updateBlocklist chatId userId Nothing
    endQuarantineForUser chatId userId

  pure status
