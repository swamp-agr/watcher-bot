module Watcher.Bot.Handle.Debug where

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Foldable (asum)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Servant.Client (runClientM)
import Telegram.Bot.API
import Telegram.Bot.API.Names
import Telegram.Bot.Simple

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as Text

import Watcher.Bot.Cache
import Watcher.Bot.Handle.Ban
import Watcher.Bot.Handle.Setup
import Watcher.Bot.Reply
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.State.Chat
import Watcher.Bot.Types
import Watcher.Bot.Utils

handleDebug :: BotState -> Update -> BotM ()
handleDebug model upd@Update{..} = do
  liftIO (log' upd)
  let mMsg = asum [updateMessage, updateEditedMessage]
  forM_ mMsg (debug model)
  pure ()

handleDebugCallback :: BotState -> CallbackQuery -> BotM ()
handleDebugCallback _model q = do
  liftIO (log' q)
  pure ()

debug :: BotState -> Message -> BotM ()
debug model msg@Message{messageText} = withDebug model $ case messageText of
  Just "setup" -> debugSetup model msg
  Just "spam" -> debugSpam model msg
  Just "getChatAdministrators" -> debugGetChatAdmins model msg
  _ -> pure ()

debugSetup :: BotState -> Message -> BotM ()
debugSetup model@BotState{..} Message{..} = do
  let mUserId = userId <$> messageFrom
      Chat{..} = messageChat
      readyOrNot = True
      group = (chatId, chatUsername)
  forM_ mUserId $ \userId' -> do
    let go Nothing = Just
          $! (newChatState botSettings) { chatAdmins = HS.singleton userId' }
        go (Just v) = Just v
    alterCache groups chatId go
    writeCache admins userId' $! HS.singleton group
  when readyOrNot $ forM_ mUserId $ \userId' -> setSingleRoot model userId' (fst group)
  replySingleGroupRoot model readyOrNot (Just chatId) (ReplySetup messageMessageId)

debugSpam :: BotState -> Message -> BotM ()
debugSpam model@BotState{..} msg = do
  now <- liftIO getCurrentTime
  liftIO $ log' ("debugSpam" :: Text)
  -- it *must* be reply to some other message
  forM_ (messageReplyToMessage msg) $ \orig -> do
    let mUserId = userId <$> messageFrom msg
        Chat{..} = messageChat msg
        group = (chatId, chatUsername)

    liftIO $ log' ("this is a reply" :: Text)

    forM_ mUserId $ \userId' -> do
      let setBan st = case messageText orig of
            Just "admins" -> st { spamCommandAction = SCAdminsCall }
            Just "poll" -> st { spamCommandAction = SCPoll, usersForConsensus = 2 }
            _ -> st
          setAdmin st = case messageText orig of
            Just "admins" -> st { chatAdmins = HS.singleton userId' }
            Just "poll" -> st { chatAdmins = HS.empty }
            _ -> st { chatAdmins = HS.singleton userId' }
          ch0 = setAdmin $ newChatState botSettings
          ch = ch0 { chatSettings = setBan (chatSettings ch0)
                   , chatSetup = SetupCompleted
                     { setupCompletedByAdmin = userId'
                     , setupCompletedAt = now
                     }
                   }
          mid = messageMessageId msg
      writeCache groups chatId ch
      writeCache admins userId' $! HS.singleton group

      handleBanAction model chatId ch (VoterId userId') mid $! messageToMessageInfo orig

debugGetChatAdmins :: BotState -> Message -> BotM ()
debugGetChatAdmins BotState{clientEnv} msg = do
  let Chat{..} = messageChat msg
  mResponse <- liftIO $ do
    eres <- flip runClientM clientEnv $ getChatAdministrators (SomeChatId chatId)
    log' eres
    case eres of
      Left _ -> pure Nothing
      Right res -> pure $! Just res
  liftIO $ log' mResponse
  forM_ mResponse $ \Response{..} -> if not responseOk
      then liftIO (log' @Text "Cannot retrieve admins")
      else do
        liftIO $ log' responseResult

handleGetChatMember :: BotState -> ChatId -> BotM ()
handleGetChatMember model@BotState{..} chatId = do
  let Settings {..} = botSettings
  forM_ ownerGroup $ \OwnerGroupSettings {} -> lookupCache groups chatId >>= \case
    Nothing -> replyText "No data for requested chat"
    Just ChatState{..} -> do
      case HM.toList quarantine of
        [] -> reply "No users in quarantine"
        xs -> do
          texts <- forM xs $ \(userId, (_mChatInfo, messageHashes)) -> do
            mResponse <- call model $ getChatMember (SomeChatId chatId) userId
            let responseToText x =
                  let cm = responseResult x
                      status = chatMemberStatus cm
                      user = makeUserLink $ chatMemberUser cm
                  in Text.concat [ user, ": ", status ]
                responseText = maybe "No response, check logs" responseToText mResponse
                userStatus = Text.concat
                  [ s2t (coerce @_ @Integer userId)
                  , ": ", responseText
                  , ", ", s2t (length messageHashes)
                  ]
            pure userStatus
          let fullChatResponse = Text.concat
                [ s2t chatId, ": ", Text.unlines texts ]
              replyMsg = (toReplyMessage fullChatResponse)
                { replyMessageParseMode = Just HTML }
          reply replyMsg
