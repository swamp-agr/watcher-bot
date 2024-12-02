module Watcher.Bot.State.User where

import Data.HashSet (HashSet)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)
import Telegram.Bot.API

import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map

import Watcher.Bot.Types
import Watcher.Orphans ()

data UserState = UserState
  { userSetupState :: UserSetupState
  , userContactState :: UserContactState
  , userCurrentState :: Maybe UserCurrentState
  }
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

newUserState :: UserState
newUserState = UserState
  { userSetupState = UserSetupNone
  , userContactState = UserContactNone
  , userCurrentState = Nothing
  }

data UserSetupState
  = UserSetupNone
  | UserSetupInProgress
      { userSetupMenu :: MenuState
      , userSetupMessageId :: Maybe MessageId
      }
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

getUserSetupMessageId :: UserSetupState -> Maybe MessageId
getUserSetupMessageId = \case
  UserSetupNone -> Nothing
  UserSetupInProgress{..} -> userSetupMessageId

data UserContactState
  = UserContactNone
  | UserContactInProgress
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

data UserCurrentState
  = UserCurrentSetup
  | UserCurrentContact
  deriving (Eq, Show, Generic, FromDhall, ToDhall)


setContactUserState :: UserState -> UserState
setContactUserState st = st
  { userContactState = UserContactInProgress
  , userCurrentState = Just UserCurrentContact
  }

completeContact :: UserState -> UserState
completeContact st@UserState{..} = if userSetupState == UserSetupNone
  then st { userContactState = UserContactNone
          , userCurrentState = Nothing
          }
  else st { userContactState = UserContactNone
          , userCurrentState = Just UserCurrentSetup
          }

setSetupUserState :: MenuState -> UserState -> UserState
setSetupUserState menu st = st
  { userSetupState = UserSetupInProgress menu Nothing
  , userCurrentState = Just UserCurrentSetup
  }

setSetupMessageUserState :: UserState -> MessageId -> UserState
setSetupMessageUserState st messageId = case userSetupState st of
  UserSetupNone -> st
  UserSetupInProgress{..} ->
    let newSetup = UserSetupInProgress { userSetupMessageId = Just messageId, userSetupMenu }
    in st { userSetupState = newSetup }

data MenuState
  = MultipleGroupsRoot { multipleGroupsMenu :: Map ChatId Text }
  | MultipleGroupsSelected
      { multipleGroupsSelectedChat :: ChatId
      , multipleGroupsSelectedSubMenu :: MenuId
      , multipleGroupsSelectedMenu :: Map ChatId Text
      }
  | SingleRoot
      { singleGroupChat :: ChatId
      , singleGroupSubMenu :: MenuId
      }
  deriving (Eq, Show, Generic, FromDhall, ToDhall)

switchMenu :: MenuState -> MenuId -> ChatId -> MenuState
switchMenu ms currentMenuId chatId = case ms of
  MultipleGroupsRoot groups -> case currentMenuId of
    Multi chatId' -> MultipleGroupsSelected
      { multipleGroupsSelectedChat = chatId'
      , multipleGroupsSelectedSubMenu = selectNextMenu currentMenuId
      , multipleGroupsSelectedMenu = groups
      }
    _ -> ms
  MultipleGroupsSelected{..} -> case currentMenuId of
    MenuRoot -> MultipleGroupsRoot multipleGroupsSelectedMenu
    Done     -> MultipleGroupsRoot multipleGroupsSelectedMenu
    _ -> MultipleGroupsSelected
      { multipleGroupsSelectedChat = chatId
      , multipleGroupsSelectedSubMenu = selectNextMenu currentMenuId
      , multipleGroupsSelectedMenu
      }
  SingleRoot {} -> SingleRoot
    { singleGroupChat = chatId
    , singleGroupSubMenu = selectNextMenu currentMenuId
    }

chatSetToMap :: HashSet (ChatId, Maybe Text) -> Map ChatId Text
chatSetToMap = Map.fromList . catMaybes . fmap transform . HS.toList
  where
    transform (chatId, Just title) = Just (chatId, title)
    transform _ = Nothing

chatMapToSet :: Map ChatId Text -> HashSet (ChatId, Maybe Text)
chatMapToSet = HS.fromList . fmap transform . Map.toList
  where
    transform (chatId, title) = (chatId, Just title)
