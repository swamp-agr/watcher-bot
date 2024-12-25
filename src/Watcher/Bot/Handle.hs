module Watcher.Bot.Handle where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import GHC.Stack (HasCallStack)
import Telegram.Bot.API
import Telegram.Bot.Simple

import Watcher.Bot.Handle.Ban
import Watcher.Bot.Handle.ChatMember
import Watcher.Bot.Handle.Contact
import Watcher.Bot.Handle.Debug
import Watcher.Bot.Handle.Dump
import Watcher.Bot.Handle.Help
import Watcher.Bot.Handle.Message
import Watcher.Bot.Handle.Setup
import Watcher.Bot.Handle.Unban
import Watcher.Bot.Reply
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.Types

-- | How to handle actions after parsing updates.
handleAction :: (WithBotState, HasCallStack) => Action -> Model -> Eff Action Model

-- | In debug we are flexible to test different things
handleAction (Debug upd) model = model <# do
  handleDebug upd

handleAction (DebugCallback q) model = model <# do
  handleDebugCallback q

handleAction (Tuning update) model = model <# do
  handleTuning update

handleAction (GetChatMember chatId) model = model <# do
  handleGetChatMember chatId 

handleAction (Analyse chatId userId message) model = model <# do
  handleAnalyseMessage chatId userId message
    
handleAction (NavigateTo userChatId messageId menuId) model = model <# do
  handleNavigate userChatId messageId menuId

-- | Setup is happening via private conversation between bot and chat admin.
-- User could be admin of multiple chats. Bot will offer a list of groups
-- where user has admin permissions.
handleAction (CheckUserSetup userId messageId) model = model <# do
  checkUserSetup userId messageId

-- | Setup must be triggered in the group where bot was invited to.
-- It simply refreshes the list of admins who can set bot up.
handleAction (CheckGroupSetup chatId _userId messageId) model = model <# do
  checkGroupSetup chatId messageId

-- | Depending on the chat setup state itself, it will send self-destruct reply with
-- the current state of setup and if setup has been completed, proceed with action
-- that has been set via 'spamCommandAction'.
handleAction (BanAction chatId userId' messageId orig) model = model <# do
  withCompletedSetup chatId $ \ch ->
    handleBanAction chatId ch (VoterId userId') messageId
      $! messageToMessageInfo orig 

handleAction (UnbanAction chatId adminId messageId someChatId) model = model <# do
  withCompletedSetup chatId $ \ch ->
    handleUnbanAction chatId ch adminId messageId someChatId

handleAction (UnbanGlobally messageId someChatId) model = model <# do
  handleGlobalUnbanAction messageId someChatId

handleAction (BotBanAction chatId botUserId bannedMember) model = model <# do
  withCompletedSetup chatId $ \ch ->
    handleBotBanAction chatId ch botUserId bannedMember

handleAction (VoteBan userId messageId voteBanId) model = model <# do
  let chatId = voteBanIdToChatId voteBanId
  withCompletedSetup chatId $ \ch ->
    handleVoteBan chatId ch (VoterId userId) messageId voteBanId

handleAction (AdminBan userId messageId adminBanId) model = model <# do
  let chatId = adminBanIdToChatId adminBanId
  withCompletedSetup chatId $ \ch ->
    handleAdminBan chatId ch userId messageId adminBanId

-- | Bot was added to the private group or channel. Those are unsupported group types now.
-- Bot will leave a message and quit the group.
handleAction (SendContactAndQuit chatId messageId) model = model <# do
  sendContactMessageAndLeave chatId messageId

-- | Bot has received @/contact@ from user's DM.
-- Bot will either forward message or ask user for it.
handleAction (ContactOwners userId message) model = model <# do
  contactOwners userId message

-- | Copy content of this message and send back to user.
handleAction (ContactUser origMsg) model = model <# do
  contactUser origMsg

-- | User sent direct message to bot, we need to check if @/contact@ state is being entered.
handleAction (CheckUserContactState userId msg) model = model <# do
  checkUserContact userId msg

handleAction (DirectMessageHelp userId messageId) model = model <# do
  directMessageReplyHelp userId messageId

handleAction (OwnerHelp messageId) model = model <# do
  replyMarkdownText messageId (ownerHelp . helpSettings . botSettings $ model)

handleAction (PublicHelp _chatId messageId) model = model <# do
  replyMarkdownText messageId (publicHelp . helpSettings . botSettings $ model)

handleAction (Dump _message) model = model <# do
  liftIO dumpAllCachesOnce

-- | Async action: part of self-destruct mechanics. Bot will delete the message if possible.
handleAction (DeleteMessage chatId messageId) model = model <# do
  void $ call $ deleteMessage chatId messageId
  pure ()

-- | Async action: check user member, if kicked, ban them everywhere too.
handleAction (CheckChatMember chatId user) model = model <# do
  void $ handleCheckChatMember chatId user
  pure ()
