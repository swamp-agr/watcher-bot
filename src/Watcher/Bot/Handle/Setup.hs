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

import Watcher.Bot.Analytics
import Watcher.Bot.Cache
import Watcher.Bot.Reply
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.State.Chat
import Watcher.Bot.State.User
import Watcher.Bot.Types
import Watcher.Bot.Utils

checkGroupSetup :: WithBotState => ChatId -> MessageId -> BotM ()
checkGroupSetup chatId messageId = do
  now <- liftIO getCurrentTime
  sendEvent (chatEvent now chatId EventGroupSetup)

  refreshChatAdmins chatId
  void $ call (deleteMessage chatId messageId)
  pure ()

checkUserSetup :: WithBotState => UserId -> MessageId -> BotM ()
checkUserSetup userId messageId = do
  let BotState {..} = ?model
  now <- liftIO getCurrentTime
  sendEvent (userEvent now userId EventUserSetup)
  -- at this point user *must be* admin.
  -- bot should be present in the group and expect @/setup@ there to fill admins cache in.
  mGroups <- lookupCache admins userId

  forM_ mGroups $ \userGroups -> case HS.size userGroups of
    -- set a single group up right away!
    1 -> withSingleGroup userGroups $ setupSingleGroup userId messageId

    -- user is admin of multiple groups, which one they want to set up?
    _ -> setupMultipleGroups userId messageId userGroups

  pure ()

handleNavigate :: WithBotState => ChatId -> MessageId -> MenuId -> BotM ()
handleNavigate userChatId messageId menuId = do
  let BotState {..} = ?model
      userId = coerce @_ @UserId userChatId
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
          sendEvent evt
          completeGroupSetup chatId userId
          replyDone (CallbackSetup messageId)

        when (setupMenu menuId) $ groupSetup chatId userId menuId
        when (menuId == BotIsAdmin) $ refreshChatAdmins chatId

        writeCache users userId newerUserState
        replyMenu newMenuState (CallbackSetup messageId) chatId userId

refreshChatAdmins :: WithBotState => ChatId -> BotM ()
refreshChatAdmins chatId = do
  let BotState {..} = ?model
  now <- liftIO getCurrentTime
  mChatState <- lookupCache groups chatId
  let st = fromMaybe (newChatState botSettings) mChatState
  when True $ do
    mResponse <- call (getChatAdministrators (SomeChatId chatId))
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

        mChatResponse <- call $ getChat (SomeChatId chatId)
        let mChatTitle = chatFullInfoTitle =<< (responseResult <$> mChatResponse)

        forM_ newChatAdmins $ \adminId -> do
          let go Nothing = Just $! HS.singleton (chatId, Nothing)
              go (Just set) = Just $! HS.insert (chatId, mChatTitle) set
          alterCache admins adminId go
  pure ()

setupMultipleGroups
  :: WithBotState => UserId -> MessageId -> HashSet (ChatId, Maybe Text) -> BotM ()
setupMultipleGroups userId messageId userGroups = do
  setMultipleRoot userId userGroups
  replyManyGroupsMenu (ReplySetup messageId) userGroups

setMultipleRoot :: WithBotState => UserId -> HashSet (ChatId, Maybe Text) -> BotM ()
setMultipleRoot userId userGroups = alterCache users userId go
  where
    BotState {..} = ?model
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

setupSingleGroup :: WithBotState => UserId -> MessageId -> (ChatId, Maybe Text) -> BotM ()
setupSingleGroup userId messageId group = do
  readyOrNot <- initGroupSetupMaybe userId group
  when readyOrNot $ setSingleRoot userId (fst group)
  
  replySingleGroupRoot readyOrNot Nothing (ReplySetup messageId)

setSingleRoot :: WithBotState => UserId -> ChatId -> BotM ()
setSingleRoot userId chatId = alterCache users userId go
  where
    BotState{..}  = ?model
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
initGroupSetupMaybe :: WithBotState => UserId -> (ChatId, Maybe Text) -> BotM Bool
initGroupSetupMaybe userId (chatId, _mChatname) = do
  let BotState {..} = ?model
  now <- liftIO getCurrentTime
  mChatState <- lookupCache groups chatId

  case mChatState of
    -- bot does not know anything about the group. that's weird!
    -- we need to retrieve chat admins just in case and make extra check
    -- whether user's admin or not.
    Nothing -> do
      mResponse <- call (getChatAdministrators (SomeChatId chatId))
      case mResponse of
        Nothing -> pure False
        Just Response{..} -> if not responseOk
          then liftIO (logT "Cannot retrieve admins") >> pure False
          else do
          let chatAdmins = membersToAdminIds responseResult
              newState = (newChatState botSettings)
                { chatAdminsCheckedAt = Just $ utctDay now
                , chatAdmins
                }
          if HS.member userId chatAdmins
            then do
              overrideChatSettings chatId newState userId now (chatSettings newState)
              pure True
            else do
              userNoLongerAdmin userId chatId
              pure False
    Just chatState@ChatState{..} -> do
      let userIsAdmin = HS.member userId chatAdmins
      if not userIsAdmin
        then pure False
        else case chatSetup of
          SetupNone -> do
            overrideChatSettings chatId chatState userId now chatSettings
            pure True
          SetupCompleted {} -> do
            overrideChatSettings chatId chatState userId now chatSettings
            pure True
          SetupInProgress {..} ->
            -- group setup is being locked by other admin and lock has not been expired yet
            if setupModifiedByAdmin /= userId && (setupModifiedAt `notLongAgoEnough` now)
              then pure False
              else do
                overrideChatSettings chatId chatState userId now chatSettings
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
userNoLongerAdmin :: WithBotState => UserId ->ChatId -> BotM ()
userNoLongerAdmin userId chatId = alterCache admins userId (go chatId)
  where
    BotState {..} = ?model
    go groupId = \case
      Nothing -> Nothing
      Just gs -> case HM.toList . HM.delete groupId . HM.fromList . HS.toList $ gs of
        [] -> Nothing
        newGroups -> Just $! HS.fromList newGroups

membersToAdminIds :: [ChatMember] -> HashSet UserId
membersToAdminIds = HS.fromList . fmap (userId . chatMemberUser)

overrideChatSettings
  :: WithBotState
  => ChatId -> ChatState
  -> UserId -> UTCTime -> GroupSettings
  -> BotM ()
overrideChatSettings chatId chatState userId now chatSettings =
  let BotState{..} = ?model
  in writeCache groups chatId $! setupChatSettings chatState userId now chatSettings

completeGroupSetup :: WithBotState => ChatId -> UserId -> BotM ()
completeGroupSetup chatId userId = do
  let BotState {..} = ?model
  now <- liftIO getCurrentTime
  alterCache groups chatId (go now)
  where
    go _time Nothing = Nothing
    go time (Just prevState) = Just $! prevState { chatSetup = SetupCompleted userId time }

groupSetup :: WithBotState => ChatId -> UserId -> MenuId -> BotM ()
groupSetup chatId userId menuId = do
  let BotState {..} = ?model
  now <- liftIO getCurrentTime
  mChatState <- lookupCache groups chatId
  let chatState = (fromMaybe (newChatState botSettings) mChatState)
      nextChatSettings = alterSettings (chatSettings chatState) menuId
  overrideChatSettings chatId chatState userId now nextChatSettings

