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

selfDestructReply :: BotState -> ChatId -> ReplyAnswerType -> BotM ()
selfDestructReply model@BotState{..} chatId answer = do
  now <- liftIO getCurrentTime
  let replyMsg = toReplyMessage (renderAnswer answer)
      req = replyMessageToSendMessageRequest (SomeChatId chatId) replyMsg
  mResponse <- call model $ sendMessage req
  forM_ mResponse $ \Response{..} -> when responseOk $ do
    let msgId = messageMessageId responseResult
        msg = SelfDestructMessage
          { selfDestructMessageChatId = chatId
          , selfDestructMessageId = msgId
          , selfDestructMessageTime = addUTCTime 20 now -- FIXME: configurable
          }
    liftIO $! atomically $! modifyTVar' selfDestructionSet $! Set.insert msg

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

withCompletedSetup :: BotState -> ChatId -> (ChatState -> BotM ()) -> BotM ()
withCompletedSetup model@BotState{..} chatId action = do
  mChatState <- lookupCache groups chatId
  -- absence of chat state should be ignored
  forM_ mChatState $ \ch@ChatState{..} -> case chatSetup of
    SetupNone -> selfDestructReply model chatId ReplyNoSetup
    SetupInProgress {} -> selfDestructReply model chatId ReplyIncompletedSetup
    SetupCompleted {} -> action ch

replyMenu :: BotState -> MenuState -> MessageId -> ChatId -> UserId -> BotM ()
replyMenu model menuState messageId chatId  _userId = case menuState of
  MultipleGroupsRoot {..} -> replyManyGroupsMenu model messageId
    $ chatMapToSet multipleGroupsMenu
  MultipleGroupsSelected {..} ->
    replySingleGroupMenu model chatId messageId multipleGroupsSelectedSubMenu
  SingleRoot {..} -> replySingleGroupMenu model chatId messageId singleGroupSubMenu

replySingleGroupMenu :: BotState -> ChatId -> MessageId -> MenuId -> BotM ()
replySingleGroupMenu model chatId messageId = \case
  MenuRoot -> replySingleGroupRoot model True False (Just chatId) messageId
  ConsensusRoot -> replyConsensusRoot model messageId
  SpamCmdRoot -> replySpamCmdRoot model messageId
  QuarantineRoot -> replyQuarantineRoot model messageId
  Done -> replyDone model messageId 
  Multi _chatId -> pure ()
  Consensus _consensus -> replyConsensusRoot model messageId
  SpamCmd _cmd -> replySpamCmdRoot model messageId
  Quarantine _messagesInQuarantine -> replyQuarantineRoot model messageId  
  BotIsAdmin -> replySingleGroupRoot model True False (Just chatId) messageId

replySingleGroupRoot
  :: HasCallStack
  => BotState -> Bool -> Bool -> Maybe ChatId -> MessageId -> BotM ()
replySingleGroupRoot  model@BotState{..} True fromCallback mChatId messageId = do
  mChatState <- maybe (pure Nothing) (lookupCache groups) mChatId
  let setupKeyboard0 = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = fmap (fmap (uncurry actionButton))
            [ [ ("Users for ban consensus", ConsensusRoot) ]
            , [ ("/spam command", SpamCmdRoot) ]
            , [ ("Quarantine duration (in messages)", QuarantineRoot) ]
            , [ ("Is Bot admin?", BotIsAdmin) ]
            , [ ("Complete", Done) ]
            ]
        }
      editMsgTxt = maybe "Choose setting to set up the group." renderChatState mChatState
      editMsg = (toEditMessage editMsgTxt)
        { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup setupKeyboard0
        }
  setupReply model fromCallback messageId editMsg
replySingleGroupRoot _model False _fromCallback _mChatId _messageId = do
  let txt = Text.concat -- FIXME: configurable
        [ "Group set up has already been initiated by some other admin. "
        , "Try tomorrow or contact other group admins"
        ]
      replyMsg = (toReplyMessage txt) 

  reply replyMsg

replyManyGroupsMenu
  :: HasCallStack => BotState -> MessageId -> HashSet (ChatId, Maybe Text) -> BotM ()
replyManyGroupsMenu model messageId adminGroups = do
  let makeButton (ChatId chatId, mchatTitle) =
        [ actionButton 
            (fromMaybe ("Untitled chat: " <> s2t chatId) mchatTitle)
            chatId
        ]
      setupKeyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = makeButton <$> HS.toList adminGroups
        }
      editMsg = (toEditMessage "Choose group to set up") -- FIXME: configurable
        { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup setupKeyboard
        }
  setupReply model False messageId editMsg

replyConsensusRoot
  :: HasCallStack => BotState -> MessageId -> BotM ()
replyConsensusRoot model messageId = do
  let makeButton ix = [ actionButton (s2t ix) (Consensus ix) ]
      keyboard = InlineKeyboardMarkup
        { inlineKeyboardMarkupInlineKeyboard = makeButton <$> [ 2 .. 7 ]
        }
        -- FIXME: configurable
      editMsg = (toEditMessage "Set up amount of users necessary for ban consensus")
       { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup keyboard
       }
  setupReply model True messageId editMsg

replySpamCmdRoot
  :: HasCallStack => BotState -> MessageId -> BotM ()
replySpamCmdRoot model messageId = do
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
  setupReply model True messageId editMsg

replyQuarantineRoot :: HasCallStack => BotState -> MessageId -> BotM ()
replyQuarantineRoot model messageId = do
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
  setupReply model True messageId editMsg

setupReply :: BotState -> Bool -> MessageId -> EditMessage -> BotM ()
setupReply model@BotState{..} fromCallback messageId editMsg = do
  mChatId <- currentChatId
  forM_ mChatId $ \chatId -> do
    if fromCallback
      then do
        let editId = EditChatMessageId (SomeChatId chatId) messageId
        void $ editMessage editId editMsg
      else 
        let userId = coerce chatId -- in DM chatId is equal to userId
        in lookupCache users userId >>= \case
          Nothing -> pure ()
          Just UserState{userSetupState} -> case getUserSetupMessageId userSetupState of
            Just menuMessageId -> do
              let editId = EditChatMessageId (SomeChatId chatId) menuMessageId
              void $ editMessage editId editMsg
            Nothing -> do
              let replyMsg = (editMessageToReplyMessage editMsg)
                    { replyMessageReplyToMessageId = Just messageId }
                  req = replyMessageToSendMessageRequest (SomeChatId chatId) replyMsg
              mResponse <- call model $ sendMessage req
              forM_ mResponse $ \Response{..} -> when responseOk $ do
                let menuMessageId = messageMessageId responseResult
                    go Nothing = Nothing
                    go (Just st) = Just $! setSetupMessageUserState st menuMessageId
                alterCache users userId go

replyDone :: HasCallStack => BotState -> MessageId -> BotM ()
replyDone model messageId = do
  let editMsg = (toEditMessage "Setup completed.")
  setupReply model False messageId editMsg

selfDestructMessages :: BotState -> (Action -> IO ()) -> IO ()
selfDestructMessages BotState{..} fun = forever $! do
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

replyStats :: BotState -> Text -> IO ()
replyStats model@BotState{..} txt = do
  let Settings {..} = botSettings
  forM_ ownerGroup $ \OwnerGroupSettings{..} -> do
    let messageReq = (defSendMessage (SomeChatId $ ChatId ownerGroupId) txt)
          { sendMessageMessageThreadId = Just $! MessageThreadId ownerGroupStatsThreadId
          }
    void $ callIO model $ sendMessage messageReq
