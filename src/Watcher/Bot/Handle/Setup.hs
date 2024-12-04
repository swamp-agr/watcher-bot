module Watcher.Bot.Handle.Setup where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime (..), getCurrentTime)
import Telegram.Bot.API
import Telegram.Bot.Simple

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map

import Watcher.Bot.Analytics
import Watcher.Bot.Cache
import Watcher.Bot.Reply
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.State.Chat
import Watcher.Bot.State.User
import Watcher.Bot.Types
import Watcher.Bot.Utils

checkGroupSetup :: BotState -> ChatId -> MessageId -> BotM ()
checkGroupSetup model chatId messageId = do
  now <- liftIO getCurrentTime
  sendEvent model (chatEvent now chatId EventGroupSetup)

  refreshChatAdmins model chatId
  void $ call model (deleteMessage chatId messageId)
  pure ()

checkUserSetup :: BotState -> UserId -> MessageId -> BotM ()
checkUserSetup model@BotState{..} userId messageId = do
  now <- liftIO getCurrentTime
  sendEvent model (userEvent now userId EventUserSetup)
  -- at this point user *must be* admin.
  -- bot should be present in the group and expect @/setup@ there to fill admins cache in.
  mGroups <- lookupCache admins userId

  forM_ mGroups $ \userGroups -> case HS.size userGroups of
    -- set a single group up right away!
    1 -> withSingleGroup userGroups $ setupSingleGroup model userId messageId

    -- user is admin of multiple groups, which one they want to set up?
    _ -> setupMultipleGroups model userId messageId userGroups

  pure ()

handleNavigate :: BotState -> ChatId -> MessageId -> MenuId -> BotM ()
handleNavigate model@BotState{..} userChatId messageId menuId = do
  let userId = coerce @_ @UserId userChatId
  -- case: Alice (admin) forwards setup to Bob who is not admin
  -- Bob clicks menu button. Here we go, catching unexpected callback :)
  --
  -- FIXME: case 2 (???): Alice is admin of Group A but not Group B.
  -- Bob forwards message with setup grup B.
  mGroups <- lookupCache admins userId
  let userIsAdmin = maybe False (not . HS.null) mGroups
  when userIsAdmin $ lookupCache users userId >>= \case
    Nothing -> pure ()
    Just us@UserState{..} -> do
      let mChatMenu = retrieveDataFromSetup userSetupState menuId
      forM_ mChatMenu $ \(chatId, prevMenu) -> do
        let newMenuState = switchMenu prevMenu menuId chatId
            newerUserState = setSetupUserState newMenuState us
        
        when (menuId == Done) $ do
          -- FIXME: check that the bot is administrator and can actually ban users
          now <- liftIO getCurrentTime
          let evt = (chatEvent now chatId EventGroupSetupCompleted)
                { eventUserId = Just userId }
          sendEvent model evt
          completeGroupSetup model chatId userId

        when (setupMenu menuId) $ groupSetup model chatId userId menuId
        when (menuId == BotIsAdmin) $ refreshChatAdmins model chatId

        writeCache users userId newerUserState
        replyMenu model newMenuState messageId chatId userId

refreshChatAdmins :: BotState -> ChatId -> BotM ()
refreshChatAdmins model@BotState{..} chatId = do
  now <- liftIO getCurrentTime
  mChatState <- lookupCache groups chatId
  let st@ChatState{..} = fromMaybe (newChatState botSettings) mChatState
      outdated = case chatAdminsCheckedAt of
        Nothing -> True
        Just day -> UTCTime day 0 `notLongAgoEnough` now
      refreshNeeded = HS.null chatAdmins || outdated
  when refreshNeeded $ do
    mResponse <- call model (getChatAdministrators (SomeChatId chatId))
    forM_ mResponse $ \response -> if not (responseOk response)
      then liftIO (log' @String "Cannot retrieve admins")
      else do
        mBot <- liftIO $ atomically $ readTVar self
        let newChatAdmins = membersToAdminIds (responseResult response)
            newState = st
              { chatAdminsCheckedAt = Just $ utctDay now
              , chatAdmins = newChatAdmins
              , botIsAdmin = maybe False (flip HS.member newChatAdmins . userInfoId) mBot
              }
        writeCache groups chatId newState

        mChatResponse <- call model $ getChat (SomeChatId chatId)
        let mChatTitle = chatFullInfoTitle =<< (responseResult <$> mChatResponse)

        forM_ newChatAdmins $ \adminId -> do
          let go Nothing = Just $! HS.singleton (chatId, Nothing)
              go (Just set) = Just $! if Map.member chatId (chatSetToMap set)
                then HS.insert (chatId, mChatTitle) set
                else set
          alterCache admins adminId go
  pure ()

setupMultipleGroups
  :: BotState -> UserId -> MessageId -> HashSet (ChatId, Maybe Text) -> BotM ()
setupMultipleGroups model userId messageId userGroups = do
  setMultipleRoot model userId userGroups
  replyManyGroupsMenu model messageId userGroups

setMultipleRoot :: BotState -> UserId -> HashSet (ChatId, Maybe Text) -> BotM ()
setMultipleRoot BotState{..} userId userGroups = alterCache users userId go
  where
    chatMap = chatSetToMap userGroups
    menu = MultipleGroupsRoot { multipleGroupsMenu = chatMap }
    go = Just . setSetupUserState menu . fromMaybe newUserState


-- | Convenient wrapper for the action for a single group.
-- Other cases will be silently discarded.
withSingleGroup
  :: HashSet (ChatId, Maybe Text)
  -> ((ChatId, Maybe Text) -> BotM ())
  -> BotM ()
withSingleGroup groups action = case HS.toList groups of
  group : [] -> action group
  _ -> pure ()

setupSingleGroup :: BotState -> UserId -> MessageId -> (ChatId, Maybe Text) -> BotM ()
setupSingleGroup model userId messageId group = do
  readyOrNot <- initGroupSetupMaybe model userId group
  when readyOrNot $ setSingleRoot model userId (fst group)
  
  replySingleGroupRoot model readyOrNot False Nothing messageId 

setSingleRoot :: BotState -> UserId -> ChatId -> BotM ()
setSingleRoot BotState{..} userId chatId = alterCache users userId go
  where
    menu =  SingleRoot
      { singleGroupChat = chatId
      , singleGroupSubMenu = MenuRoot
      }
    go = Just . setSetupUserState menu . fromMaybe newUserState

-- | Inspecting setup state for the given group by admin:
-- 
-- * None - admin might start set up.
-- * InProgress - either initiated by either current or some other admin.
-- * Completed - it could be modified again.
--
initGroupSetupMaybe :: BotState -> UserId -> (ChatId, Maybe Text) -> BotM Bool
initGroupSetupMaybe model@BotState{..} userId (chatId, _mChatname) = do
  now <- liftIO getCurrentTime
  mChatState <- lookupCache groups chatId

  case mChatState of
    -- bot does not know anything about the group. that's weird!
    -- we need to retrieve chat admins just in case and make extra check
    -- whether user's admin or not.
    Nothing -> do
      mResponse <- call model (getChatAdministrators (SomeChatId chatId))
      case mResponse of
        Nothing -> pure False
        Just Response{..} -> if not responseOk
          then liftIO (log' @String "Cannot retrieve admins") >> pure False
          else do
          let chatAdmins = membersToAdminIds responseResult
              newState = (newChatState botSettings)
                { chatAdminsCheckedAt = Just $ utctDay now
                , chatAdmins
                }
          if HS.member userId chatAdmins
            then do
              overrideChatSettings model chatId newState userId now (chatSettings newState)
              pure True
            else do
              userNoLongerAdmin model userId chatId
              pure False
    Just chatState@ChatState{..} -> do
      let userIsAdmin = HS.member userId chatAdmins
      if not userIsAdmin
        then pure False
        else case chatSetup of
          SetupNone -> do
            overrideChatSettings model chatId chatState userId now chatSettings
            pure True
          SetupCompleted {} -> do
            overrideChatSettings model chatId chatState userId now chatSettings
            pure True
          SetupInProgress {..} ->
            -- group setup is being locked by other admin and lock has not been expired yet
            if setupModifiedByAdmin /= userId && (setupModifiedAt `notLongAgoEnough` now)
              then pure False
              else do
                overrideChatSettings model chatId chatState userId now chatSettings
                pure True

retrieveDataFromSetup :: UserSetupState -> MenuId -> Maybe (ChatId, MenuState)
retrieveDataFromSetup setupState menu = case setupState of
  UserSetupNone -> Nothing
  UserSetupInProgress menuState _ -> case menuState of
    MultipleGroupsRoot{} -> case menu of
      Multi chatId -> Just (chatId, menuState)
      _            -> Nothing
    MultipleGroupsSelected{..} -> Just (multipleGroupsSelectedChat, menuState)
    SingleRoot{..} -> Just (singleGroupChat, menuState)

-- | If user was admin of a single group but not anymore,
-- we discard cache for this user entirely.
userNoLongerAdmin :: BotState -> UserId ->ChatId -> BotM ()
userNoLongerAdmin BotState{..} userId chatId = alterCache admins userId (go chatId)
  where
    go groupId = \case
      Nothing -> Nothing
      Just gs -> case HM.toList . HM.delete groupId . HM.fromList . HS.toList $ gs of
        [] -> Nothing
        newGroups -> Just $! HS.fromList newGroups

membersToAdminIds :: [ChatMember] -> HashSet UserId
membersToAdminIds = HS.fromList . fmap (userId . chatMemberUser)

overrideChatSettings
  :: BotState
  -> ChatId -> ChatState
  -> UserId -> UTCTime -> GroupSettings
  -> BotM ()
overrideChatSettings BotState{..} chatId chatState userId now chatSettings = 
  writeCache groups chatId $! setupChatSettings chatState userId now chatSettings

completeGroupSetup :: BotState -> ChatId -> UserId -> BotM ()
completeGroupSetup BotState{..} chatId userId = do
  now <- liftIO getCurrentTime
  alterCache groups chatId (go now)
  where
    go _time Nothing = Nothing
    go time (Just prevState) = Just $! prevState { chatSetup = SetupCompleted userId time }

groupSetup :: BotState -> ChatId -> UserId -> MenuId -> BotM ()
groupSetup model@BotState{..} chatId userId menuId = do
  now <- liftIO getCurrentTime
  mChatState <- lookupCache groups chatId
  let chatState = (fromMaybe (newChatState botSettings) mChatState)
      nextChatSettings = alterSettings (chatSettings chatState) menuId
  overrideChatSettings model chatId chatState userId now nextChatSettings

