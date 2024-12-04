module Watcher.Bot.Types.Action where

import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)
import Telegram.Bot.API

import Watcher.Bot.Settings
import Watcher.Bot.Types.Common

data VoteBanId
  = VoteForBan ChatId SpamerId
  | VoteAgainstBan ChatId SpamerId
  deriving (Eq, Show, Read)

voteBanIdToSpamerId :: VoteBanId -> SpamerId
voteBanIdToSpamerId = \case
  VoteForBan _ sid -> sid
  VoteAgainstBan _ sid -> sid

voteBanIdToChatId :: VoteBanId -> ChatId
voteBanIdToChatId = \case
  VoteForBan cid _ -> cid
  VoteAgainstBan cid _ -> cid

data AdminBanId
  = AdminForBan ChatId SpamerId
  | AdminAgainstBan ChatId SpamerId
  deriving (Eq, Show, Read)

adminBanIdToChatId :: AdminBanId -> ChatId
adminBanIdToChatId = \case
  AdminForBan cid _ -> cid
  AdminAgainstBan cid _ -> cid

adminBanIdToSpamerId :: AdminBanId -> SpamerId
adminBanIdToSpamerId = \case
  AdminForBan _ sid -> sid
  AdminAgainstBan _ sid -> sid

data MenuId
  -- root level
  = MenuRoot
  -- single group
  | ConsensusRoot
  | SpamCmdRoot
  | QuarantineRoot
  | Done
  -- multiple group
  | Multi ChatId
  -- Consensus
  | Consensus Int
  -- Ban 
  | SpamCmd SpamCommand
  -- Quarantine
  | Quarantine Int
  -- Is Bot Admin
  | BotIsAdmin
  deriving (Eq, Show, Read, Generic, FromDhall, ToDhall)

selectNextMenu :: MenuId -> MenuId
selectNextMenu = \case
  Consensus _ -> MenuRoot
  SpamCmd _ -> MenuRoot
  Quarantine _ -> MenuRoot
  BotIsAdmin -> MenuRoot
  Done -> MenuRoot
  x -> x

setupMenu :: MenuId -> Bool
setupMenu = \case
  Consensus  x -> x `elem` [ 2 .. 7 ]
  SpamCmd    _  -> True
  Quarantine x -> x `elem` [ 1 .. 5 ]
  _            -> False

alterSettings :: GroupSettings -> MenuId -> GroupSettings
alterSettings gs = \case
  Consensus new -> gs { usersForConsensus = fromIntegral new }
  SpamCmd new -> gs { spamCommandAction = new }
  Quarantine new -> gs { messagesInQuarantine = fromIntegral new }
  _ -> gs


data Action
  -- setup
  = CheckUserSetup UserId MessageId
  | CheckGroupSetup ChatId UserId MessageId

  -- ban/undo
  | BanAction ChatId UserId MessageId Message
  | UnbanAction ChatId UserId MessageId SomeChatId 

  -- contact
  | SendContactAndQuit ChatId MessageId
  | ContactOwners UserId Message
  | ContactUser Message
  | CheckUserContactState UserId Message

  -- setup callbacks
  | NavigateTo ChatId MessageId MenuId

  -- ban callbacks
  | VoteBan UserId MessageId VoteBanId
  | AdminBan UserId MessageId AdminBanId

  -- Background
  | DeleteMessage ChatId MessageId

  -- Help
  | DirectMessageHelp UserId MessageId
  | OwnerHelp MessageId
  | PublicHelp ChatId MessageId
  
  -- Message
  | Analyse ChatId UserId Message
  | Tuning Update

  -- Dump
  | Dump Message

  -- debug
  | Debug Update 
  | DebugCallback CallbackQuery
  deriving Show
