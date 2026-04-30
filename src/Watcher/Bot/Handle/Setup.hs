module Watcher.Bot.Handle.Setup where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Time (UTCTime (..), getCurrentTime)
import Telegram.Bot.API
import Telegram.Bot.Simple

import qualified Data.HashSet as HS
import qualified Data.Vector.Hashtables as HT

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
  mGroups <- liftIO $ lookupCache admins userId

  forM_ mGroups $ \userGroups -> (liftIO $ HT.size userGroups) >>= \case
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
  userIsAdmin <- maybe (pure False) (liftIO . fmap not . HT.null) mGroups
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
  ncs <- newChatState botSettings
  let st = fromMaybe ncs mChatState
  when True $ do
    mResponse <- call (getChatAdministrators (SomeChatId chatId))
    forM_ mResponse $ \response -> if not (responseOk response)
      then liftIO (log' @String "Cannot retrieve admins")
      else do
        mBot <- liftIO $ atomically $ readTVar self
        newChatAdmins <- liftIO $ membersToAdminIds (responseResult response)
        botIsAdmin' <- case mBot of
          Nothing -> pure False
          Just bot -> liftIO (isJust <$> HT.lookup newChatAdmins (userInfoId bot))
        let newState = st
              { chatStateAdminsCheckedAt = Just $ utctDay now
              , chatStateAdmins = newChatAdmins
              , chatStateBotIsAdmin = botIsAdmin'
              }
        liftIO $ writeCache groups chatId newState

        mChatResponse <- call $ getChat (SomeChatId chatId)
        let mChatTitle = chatFullInfoTitle =<< (responseResult <$> mChatResponse)

        adminList <- liftIO $ HT.toList newChatAdmins
        forM_ (fst <$> adminList) \adminId -> liftIO $ do
          newAdmin <- lookupCache admins adminId >>= \case
            Nothing -> toHSet $ HS.singleton (chatId, mChatTitle)
            Just prevAdmins -> do
              HT.insert prevAdmins (chatId, mChatTitle) ()
              pure prevAdmins
          writeCache admins adminId newAdmin
  pure ()

setupMultipleGroups
  :: WithBotState => UserId -> MessageId -> HashSet (ChatId, Maybe Text) -> BotM ()
setupMultipleGroups userId messageId userGroups = do
  setMultipleRoot userId userGroups
  replyManyGroupsMenu (ReplySetup messageId) userGroups

setMultipleRoot :: WithBotState => UserId -> HashSet (ChatId, Maybe Text) -> BotM ()
setMultipleRoot userId userGroups = do
  let BotState {..} = ?model
  chatMap <- liftIO $ chatSetToMap userGroups
  let menu = MultipleGroupsRoot { multipleGroupsMenu = chatMap }
      go = Just . setSetupUserState menu . fromMaybe newUserState
  liftIO $ alterCache users userId go

-- | Convenient wrapper for the action for a single group.
-- Other cases will be silently discarded.
withSingleGroup
  :: HashSet (ChatId, Maybe Text)
  -> ((ChatId, Maybe Text) -> BotM ())
  -> BotM ()
withSingleGroup groups action = (liftIO $ HT.toList groups) >>= \case
  group : [] -> action $ fst group
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
          chatStateAdmins <- membersToAdminIds responseResult
          ncs <- liftIO $ newChatState botSettings
          let newState = ncs
                { chatStateAdminsCheckedAt = Just $ utctDay now
                , chatStateAdmins
                }
          userIsAdmin <- liftIO (isJust <$> HT.lookup chatStateAdmins userId)
          if userIsAdmin
            then do
              overrideChatSettings chatId newState userId now (chatStateSettings newState)
              pure True
            else do
              userNoLongerAdmin userId chatId
              pure False
    Just chatState@ChatState{..} -> do
      userIsAdmin <- liftIO (isJust <$> HT.lookup chatStateAdmins userId)
      if not userIsAdmin
        then pure False
        else case chatStateSetup of
          SetupNone -> do
            overrideChatSettings chatId chatState userId now chatStateSettings
            pure True
          SetupCompleted {} -> do
            overrideChatSettings chatId chatState userId now chatStateSettings
            pure True
          SetupInProgress {..} ->
            -- group setup is being locked by other admin and lock has not been expired yet
            if setupModifiedByAdmin /= userId && (setupModifiedAt `notLongAgoEnough` now)
              then pure False
              else do
                overrideChatSettings chatId chatState userId now chatStateSettings
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
userNoLongerAdmin :: WithBotState => UserId -> ChatId -> BotM ()
userNoLongerAdmin userId chatId = liftIO do
  let BotState{..} = ?model
  mGroups <- lookupCache admins userId
  forM_ mGroups \(gset :: HashSet (ChatId, Maybe Text)) -> do
    gs <- fromHSet gset
    ngset <- toHSet $ HS.filter ((/= chatId) . fst) gs
    writeCache admins userId ngset

membersToAdminIds :: MonadIO m => [ChatMember] -> m (HashSet UserId)
membersToAdminIds = liftIO . toHSet . HS.fromList . fmap (userId . chatMemberUser)

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
    go time (Just prevState) = Just
      $! prevState { chatStateSetup = SetupCompleted userId time }

groupSetup :: WithBotState => ChatId -> UserId -> MenuId -> BotM ()
groupSetup chatId userId menuId = do
  let BotState {..} = ?model
  now <- liftIO getCurrentTime
  mChatState <- lookupCache groups chatId
  ncs <- liftIO $ newChatState botSettings
  let chatState = fromMaybe ncs mChatState
      nextChatSettings = alterSettings (chatStateSettings chatState) menuId
  overrideChatSettings chatId chatState userId now nextChatSettings
