module Watcher.Bot.Handle.Help where

import Data.Text (Text)
import Telegram.Bot.API
import Telegram.Bot.Simple

import Watcher.Bot.Cache
import Watcher.Bot.Settings
import Watcher.Bot.State

directMessageReplyHelp :: BotState -> UserId -> MessageId -> BotM ()
directMessageReplyHelp BotState{..} userId messageId = do
  let Settings{..} = botSettings
      HelpSettings{..} = helpSettings
  lookupCache admins userId >>= \case
    Nothing -> replyMarkdownText messageId publicHelp
    Just _  -> replyMarkdownText messageId adminHelp

replyMarkdownText :: MessageId -> Text -> BotM ()
replyMarkdownText messageId txt = do
  let replyMsg = (toReplyMessage txt)
        { replyMessageReplyToMessageId = Just messageId
        , replyMessageParseMode = Just MarkdownV2
        }
  reply replyMsg
