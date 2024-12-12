module Watcher.Bot.Handle where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import GHC.Stack (HasCallStack)
import Telegram.Bot.API
import Telegram.Bot.Simple

import Watcher.Bot.Handle.Ban
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
handleAction :: HasCallStack => Action -> Model -> Eff Action Model

-- | In debug we are flexible to test different things
handleAction (Debug upd) model = model <# do
  handleDebug model upd

handleAction (DebugCallback q) model = model <# do
  handleDebugCallback model q

handleAction (Tuning update) model = model <# do
  handleTuning model update

handleAction (Analyse chatId userId message) model = model <# do
  handleAnalyseMessage model chatId userId message
    
handleAction (NavigateTo userChatId messageId menuId) model = model <# do
  handleNavigate model userChatId messageId menuId

-- | Setup is happening via private conversation between bot and chat admin.
-- User could be admin of multiple chats. Bot will offer a list of groups
-- where user has admin permissions.
handleAction (CheckUserSetup userId messageId) model = model <# do
  checkUserSetup model userId messageId

-- | Setup must be triggered in the group where bot was invited to.
-- It simply refreshes the list of admins who can set bot up.
handleAction (CheckGroupSetup chatId _userId messageId) model = model <# do
  checkGroupSetup model chatId messageId

-- | Depending on the chat setup state itself, it will send self-destruct reply with
-- the current state of setup and if setup has been completed, proceed with action
-- that has been set via 'spamCommandAction'.
handleAction (BanAction chatId userId' messageId orig) model = model <# do
  withCompletedSetup model chatId $ \ch ->
    handleBanAction model chatId ch (VoterId userId') messageId
      $! messageToMessageInfo orig 

handleAction (UnbanAction chatId adminId messageId someChatId) model = model <# do
  withCompletedSetup model chatId $ \ch ->
    handleUnbanAction model chatId ch adminId messageId someChatId

handleAction (BotBanAction chatId botUserId bannedMember) model = model <# do
  withCompletedSetup model chatId $ \ch ->
    handleBotBanAction model chatId ch botUserId bannedMember

handleAction (VoteBan userId messageId voteBanId) model = model <# do
  let chatId = voteBanIdToChatId voteBanId
  withCompletedSetup model chatId $ \ch ->
    handleVoteBan model chatId ch (VoterId userId) messageId voteBanId

handleAction (AdminBan userId messageId adminBanId) model = model <# do
  let chatId = adminBanIdToChatId adminBanId
  withCompletedSetup model chatId $ \ch ->
    handleAdminBan model chatId ch userId messageId adminBanId

-- | Bot was added to the private group or channel. Those are unsupported group types now.
-- Bot will leave a message and quit the group.
handleAction (SendContactAndQuit chatId messageId) model = model <# do
  sendContactMessageAndLeave model chatId messageId

-- | Bot has received @/contact@ from user's DM.
-- Bot will either forward message or ask user for it.
handleAction (ContactOwners userId message) model = model <# do
  contactOwners model userId message

-- | Copy content of this message and send back to user.
handleAction (ContactUser origMsg) model = model <# do
  contactUser model origMsg

-- | User sent direct message to bot, we need to check if @/contact@ state is being entered.
handleAction (CheckUserContactState userId msg) model = model <# do
  checkUserContact model userId msg

handleAction (DirectMessageHelp userId messageId) model = model <# do
  directMessageReplyHelp model userId messageId

handleAction (OwnerHelp messageId) model = model <# do
  replyMarkdownText messageId (ownerHelp . helpSettings . botSettings $ model)

handleAction (PublicHelp _chatId messageId) model = model <# do
  replyMarkdownText messageId (publicHelp . helpSettings . botSettings $ model)

handleAction (Dump _message) model = model <# do
  liftIO $ dumpAllCachesOnce model

-- | Async action: part of self-destruct mechanics. Bot will delete the message if possible.
handleAction (DeleteMessage chatId messageId) model = model <# do
  void $ call model $ deleteMessage chatId messageId
  pure ()

