module Watcher.Bot.Reply where

import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Control.Monad (forever, forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (addUTCTime, diffUTCTime, getCurrentTime)
import GHC.Stack (HasCallStack)
import Telegram.Bot.API
import Telegram.Bot.Simple

import qualified Data.HashSet as HS
import qualified Data.Set as Set
import qualified Data.Text as Text

import Watcher.Bot.Cache
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.State.Chat
import Watcher.Bot.State.User
import Watcher.Bot.Types
import Watcher.Bot.Utils

sendUnsupportedMessage :: MessageId -> BotM ()
sendUnsupportedMessage messageId = do
  let msg = Text.concat
        [ "I am not supporting private groups or channels yet. "
        ,"Use /contact to reach my creators."
        ]
      replyMsg = (toReplyMessage msg) { replyMessageReplyToMessageId = Just messageId }
  reply replyMsg

data ReplyAnswerType
  = ReplyNoSetup
  | ReplyIncompletedSetup
  | ReplyUserHasNotBeenBanned { replyUserHasNotBeenBanned :: UserInfo }
  | ReplyUserHasBeenUnbanned { replyUserHasBeenUnbanned :: UserInfo }
  | ReplyUserAlreadyBanned { replyUserAlreadyBanned :: UserInfo }
  | ReplyUserRecovered { replyUserRecovered :: UserInfo }
  | ReplyConsensus Int

renderAnswer :: ReplyAnswerType -> Text
renderAnswer = \case
  ReplyNoSetup -> Text.concat
    [ "Bot setup has not been started. "
    , "Please run it via /setup command in the group"
    ]
  ReplyIncompletedSetup -> Text.concat
    [ "Bot setup has not been completed yet. "
    , "Please consult with the administrator who started setup."
    ]
  ReplyUserAlreadyBanned u ->
    Text.concat
      [ "User "
      , userInfoLink u
      , " has been recognised as a known spammer and been removed from the group."
      ]
  ReplyUserRecovered u ->
    Text.concat
      [ "User "
      , userInfoLink u
      , " has not been recognised as a spamer."
      ]
  ReplyConsensus n ->
    Text.concat
      [ "Poll closed. User is banned with "
      , s2t n
      , " votes."
      ]
  ReplyUserHasNotBeenBanned u ->
    Text.concat
      [ "User "
      , userInfoLink u
      , " has not been banned in this group."
      ]
  ReplyUserHasBeenUnbanned u ->
    Text.concat
      [ "User "
      , userInfoLink u
      , " has been successfully unbanned."
      ]

selfDestructReply :: WithBotState => ChatId -> ChatState -> ReplyAnswerType -> BotM ()
selfDestructReply chatId ChatState{..} answer = when selfDestroyEnabled $ do
  let BotState {..} = ?model
  now <- liftIO getCurrentTime
  let replyMsg = (toReplyMessage (renderAnswer answer))
        { replyMessageParseMode = Just HTML }
      req = replyMessageToSendMessageRequest (SomeChatId chatId) replyMsg
  mResponse <- call $ sendMessage req
  forM_ mResponse $ \Response{..} -> when responseOk $ do
    let msgId = messageMessageId responseResult
        msg = SelfDestructMessage
          { selfDestructMessageChatId = chatId
          , selfDestructMessageId = msgId
          , selfDestructMessageTime = addUTCTime 20 now -- FIXME: configurable
          }
    liftIO $! atomically $! modifyTVar' selfDestructionSet $! Set.insert msg
  where
    GroupSettings{..} = chatSettings

replyCallAdmins :: ChatId -> SpamerId -> [Text] -> MessageInfo -> BotM ()
replyCallAdmins chatId spamerId adminUsernames originalMessage = do
  let makeButton = uncurry actionButton
      keyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard =
            [ makeButton <$> [ ("Yes", AdminForBan chatId spamerId), ("No", AdminAgainstBan chatId spamerId) ] ]
        }
      replyMsgTxt = Text.concat
        [ Text.intercalate ", " adminUsernames
        , ": is it a spamer?"
        ]
      replyMsg = (toReplyMessage replyMsgTxt)
        { replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup keyboard
        , replyMessageReplyToMessageId = Just $ messageInfoId originalMessage
        , replyMessageParseMode = Just HTML -- admin name might be a link
        }
  reply replyMsg

withCompletedSetup :: WithBotState => ChatId -> (ChatState -> BotM ()) -> BotM ()
withCompletedSetup chatId action = do
  let BotState {..} = ?model
  mChatState <- lookupCache groups chatId
  -- absence of chat state should be ignored
  forM_ mChatState $ \ch@ChatState{..} -> case chatSetup of
    SetupNone -> selfDestructReply chatId ch ReplyNoSetup
    SetupInProgress {} -> selfDestructReply chatId ch ReplyIncompletedSetup
    SetupCompleted {} -> action ch

-- | This one is coming from callback!
replyMenu :: WithBotState => MenuState -> SetupMessageId -> ChatId -> UserId -> BotM ()
replyMenu menuState messageId chatId  _userId = case menuState of
  MultipleGroupsRoot {..} -> replyManyGroupsMenu messageId $ chatMapToSet multipleGroupsMenu
  MultipleGroupsSelected {..} ->
    replySingleGroupMenu chatId messageId multipleGroupsSelectedSubMenu
  SingleRoot {..} -> replySingleGroupMenu chatId messageId singleGroupSubMenu

replySingleGroupMenu :: WithBotState => ChatId -> SetupMessageId -> MenuId -> BotM ()
replySingleGroupMenu chatId messageId = \case
  MenuRoot -> replySingleGroupRoot True (Just chatId) messageId
  ConsensusRoot -> replyConsensusRoot messageId
  SpamCmdRoot -> replySpamCmdRoot messageId
  QuarantineRoot -> replyQuarantineRoot messageId
  SelfDestroyRoot -> replySelfDestroyRoot messageId
  Done -> replyDone messageId 
  Multi selectedChatId -> replySingleGroupRoot True (Just selectedChatId) messageId
  Consensus _consensus -> replyConsensusRoot messageId
  SpamCmd _cmd -> replySpamCmdRoot messageId
  Quarantine _messagesInQuarantine -> replyQuarantineRoot messageId  
  SelfDestroy _enabled -> replySelfDestroyRoot messageId  
  BotIsAdmin -> replySingleGroupRoot True (Just chatId) messageId

replySingleGroupRoot
  :: WithBotState => HasCallStack
  => Bool -> Maybe ChatId -> SetupMessageId -> BotM ()
replySingleGroupRoot True mChatId messageId = do
  let BotState {..} = ?model
  mChatState <- maybe (pure Nothing) (lookupCache groups) mChatId
  let setupKeyboard0 = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = fmap (fmap (uncurry actionButton))
            [ [ ("Users for ban consensus", ConsensusRoot) ]
            , [ ("/spam command", SpamCmdRoot) ]
            , [ ("Quarantine duration (in messages)", QuarantineRoot) ]
            , [ ("Is Bot admin?", BotIsAdmin) ]
            , [ ("Self-destroyable messages", SelfDestroyRoot) ]
            , [ ("Complete", Done) ]
            ]
        }
      editMsgTxt = maybe "Choose setting to set up the group." renderChatState mChatState
      editMsg = (toEditMessage editMsgTxt)
        { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup setupKeyboard0
        }
  setupReply messageId editMsg
replySingleGroupRoot False _mChatId _messageId = do
  let txt = Text.concat -- FIXME: configurable
        [ "Group set up has already been initiated by some other admin. "
        , "Try tomorrow or contact other group admins"
        ]
      replyMsg = (toReplyMessage txt) 

  reply replyMsg

replyManyGroupsMenu
  :: WithBotState => HasCallStack
  => SetupMessageId -> HashSet (ChatId, Maybe Text) -> BotM ()
replyManyGroupsMenu messageId adminGroups = do
  let makeButton (chatId, mchatTitle) =
        [ actionButton 
            (fromMaybe ("Untitled chat: " <> s2t chatId) mchatTitle)
            (Multi chatId)
        ]
      setupKeyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = makeButton <$> HS.toList adminGroups
        }
      editMsg = (toEditMessage "Choose group to set up") -- FIXME: configurable
        { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup setupKeyboard
        }
  setupReply messageId editMsg

replyConsensusRoot
  :: WithBotState => HasCallStack => SetupMessageId -> BotM ()
replyConsensusRoot messageId = do
  let makeButton ix = [ actionButton (s2t ix) (Consensus ix) ]
      keyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = makeButton <$> [ 2 .. 7 ]
        }
        -- FIXME: configurable
      editMsg = (toEditMessage "Set up amount of users necessary for ban consensus")
       { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup keyboard
       }
  setupReply messageId editMsg

replySpamCmdRoot
  :: WithBotState => HasCallStack => SetupMessageId -> BotM ()
replySpamCmdRoot messageId = do
  let makeButton (label, action) =
        [ actionButton label (SpamCmd action) ]
      keyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = makeButton
          <$> [ ("Create poll", SCPoll)
              , ("Call admins", SCAdminsCall)
              ]
        }
      -- FIXME: configurable
      editMsg = (toEditMessage "Set up /spam command")
        { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup keyboard
        }
  setupReply messageId editMsg

replyQuarantineRoot :: WithBotState => HasCallStack => SetupMessageId -> BotM ()
replyQuarantineRoot messageId = do
  let makeButton ix = [ actionButton (s2t ix) (Quarantine ix) ]
      keyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = makeButton <$> [ 1 .. 5 ] }
      msg = Text.unlines
        [ "Set up quarantine mode for every user bot is listening to."
        , "Once group setup has finished, bot will start to monitor all users' messages."
        , "This setting is about amount of messages to watch. "
        , "Once limit is reached, user is no longer in \"quarantine\"."
        ]
      editMsg = (toEditMessage msg)
        { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup keyboard
        }
  setupReply messageId editMsg

replySelfDestroyRoot :: WithBotState => HasCallStack => SetupMessageId -> BotM ()
replySelfDestroyRoot messageId = do
  let makeButton (label, action) =
        [ actionButton label (SelfDestroy action) ]
      keyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = makeButton
          <$> [ ("Enabled", True)
              , ("Disabled", False)
              ]
        }
      -- FIXME: configurable
      editMsg = (toEditMessage "Self-destroyable messages")
        { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup keyboard
        }
  setupReply messageId editMsg

setupReply :: WithBotState => SetupMessageId -> EditMessage -> BotM ()
setupReply setupMessageId editMsg = do
  let BotState {..} = ?model
  mChatId <- currentChatId
  forM_ mChatId $ \chatId -> case setupMessageId of
    CallbackSetup messageId -> do
      let editId = EditChatMessageId (SomeChatId chatId) messageId
      void $ editMessage editId editMsg
    ReplySetup messageId -> do
      let userId = coerce chatId -- in DM chatId is equal to userId
      lookupCache users userId >>= \case
        Nothing -> pure ()
        Just UserState{userSetupState} -> case getUserSetupMessageId userSetupState of
          Just menuMessageId -> do
            let editId = EditChatMessageId (SomeChatId chatId) menuMessageId
            void $ editMessage editId editMsg
          Nothing -> do
            let replyMsg = (editMessageToReplyMessage editMsg)
                  { replyMessageReplyToMessageId = Just messageId }
                req = replyMessageToSendMessageRequest (SomeChatId chatId) replyMsg
            mResponse <- call $ sendMessage req
            forM_ mResponse $ \Response{..} -> when responseOk $ do
              let menuMessageId = messageMessageId responseResult
                  go Nothing = Nothing
                  go (Just st) = Just $! setSetupMessageUserState st menuMessageId
              alterCache users userId go

replyDone :: WithBotState => HasCallStack => SetupMessageId -> BotM ()
replyDone messageId = do
  let editMsg = (toEditMessage "Setup completed.")
  setupReply messageId editMsg

selfDestructMessages :: WithBotState => (Action -> IO ()) -> IO ()
selfDestructMessages fun = forever $! do
  let BotState{..} = ?model
  queue <- atomically $ readTVar selfDestructionSet
  case Set.lookupMin queue of
    Nothing -> wait 1
    Just ad@SelfDestructMessage{..} -> do
      atomically $! modifyTVar' selfDestructionSet $! Set.delete ad
      now <- getCurrentTime
      if selfDestructMessageTime < now
        then fun (DeleteMessage selfDestructMessageChatId selfDestructMessageId)
        else do
          let sec = diffUTCTime selfDestructMessageTime now
          wait $ round sec
          fun (DeleteMessage selfDestructMessageChatId selfDestructMessageId)

replyStats :: WithBotState => Text -> IO ()
replyStats txt = do
  let BotState{botSettings} = ?model
  let Settings {..} = botSettings
  forM_ ownerGroup $ \OwnerGroupSettings{..} -> do
    let messageReq = (defSendMessage (SomeChatId $ ChatId ownerGroupId) txt)
          { sendMessageMessageThreadId = Just $! MessageThreadId ownerGroupStatsThreadId
          }
    void $ callIO $ sendMessage messageReq
