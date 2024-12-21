module Watcher.Bot.Handle.Contact where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, join, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit, isSpace)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time (getCurrentTime)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Text.Read (readMaybe)

import qualified Data.Text as Text

import Watcher.Bot.Analytics
import Watcher.Bot.Cache
import Watcher.Bot.Reply
import Watcher.Bot.Settings
import Watcher.Bot.State
import Watcher.Bot.State.User
import Watcher.Bot.Utils

checkUserContact :: WithBotState => UserId -> Message -> BotM ()
checkUserContact userId msg = do
  let BotState {..} = ?model
  lookupCache users userId >>= \case
    Nothing -> do
      alterCache users userId (const $ Just newUserState)
      -- fun part
      forM_ (messageText msg) \txt -> when (Text.isPrefixOf "/" txt) $ do
        now <- liftIO getCurrentTime
        let evt = (userEvent now userId EventUserPrivateCommand)
              { eventData = listToMaybe (Text.words txt) }
        sendEvent evt
    Just UserState{..} -> when (userCurrentState == Just UserCurrentContact) $ do
      let userChatId = coerce @UserId @ChatId userId
      forwardToOwnersMaybe  Feedback userChatId $ messageMessageId msg
      alterCache users userId (maybe (Just newUserState) (Just . completeContact))
      replyText "Your message has been sent. We'll come back to you."

contactUser :: WithBotState => Message -> BotM ()
contactUser origMsg = do
  forM_ (messageReplyToMessage origMsg) $ \msg -> do
    let fromChatId = chatId $ messageChat origMsg
        tryExtractChatIdFromForward m = join $ forM (messageForwardOrigin m) $ \case
          MessageOriginUser{messageOriginUserSenderUser} ->
            let userId' = userId messageOriginUserSenderUser
            in pure $ coerce @_ @ChatId userId'
          _ -> Nothing
        tryExtractChatIdFromText
          = fmap (Text.unpack . Text.takeWhile isDigit . Text.dropWhile (not . isDigit))
          . listToMaybe . filter (Text.isPrefixOf "chatId:") . Text.lines
        tryExtractChatIdFromReply m
          = fmap ChatId . readMaybe @Integer =<< tryExtractChatIdFromText =<< messageText m
        mChatId = tryExtractChatIdFromForward msg <|> tryExtractChatIdFromReply msg
    liftIO $ log' (tryExtractChatIdFromForward msg, tryExtractChatIdFromReply msg, msg)
    case mChatId of
      Nothing -> do
        let replyMsg = (toReplyMessage "Unable to retrieve ChatId")
              { replyMessageReplyToMessageId = Just $ messageMessageId origMsg
              , replyMessageMessageThreadId = messageMessageThreadId origMsg
              }
        reply replyMsg
      Just chatId -> do
        let copyMessageReq =
              defCopyMessage
                (SomeChatId chatId) (SomeChatId fromChatId) (messageMessageId origMsg)
        void $ call $ copyMessage copyMessageReq

collectFeedback :: WithBotState => ChatId -> BotM ()
collectFeedback chatId = do
  let BotState {..} = ?model
  mResponse <- call (getChat $ SomeChatId chatId)
  forM_ mResponse $ \Response{..} -> when responseOk $ do
    let Settings{..} = botSettings
        ChatFullInfo{..} = responseResult
        msg = Text.unlines
          [ "chatId: " <> s2t (coerce @_ @Integer chatId)
          , "title: ", maybe "" (Text.cons '@') chatFullInfoTitle
          , "username: ", maybe "" (Text.cons '@') chatFullInfoUsername
          , "type: ", s2t chatFullInfoType
          ]
    liftIO $ logT msg
    forM_ ownerGroup $ \OwnerGroupSettings{..} -> do
      let sendMessageRequest = (defSendMessage (SomeChatId $ ChatId ownerGroupId) msg)
            { sendMessageMessageThreadId = Just $ MessageThreadId ownerGroupFeedbackThreadId
            }
      void $ call (sendMessage sendMessageRequest)
      pure ()
  pure ()

contactOwners :: WithBotState => UserId -> Message -> BotM ()
contactOwners userId message = do
  let BotState {..} = ?model
  let chatId = coerce @UserId @ChatId userId
  collectFeedback chatId
  let txt = Text.drop 1 . Text.dropWhile (not . isSpace) . fromMaybe "" . messageText
        $ message
  if not (Text.null txt)
    then do
      forwardToOwnersMaybe Feedback chatId $ messageMessageId message
      alterCache users userId (maybe (Just newUserState) (Just . completeContact))
      replyText "Your message has been sent. We'll come back to you."
    else do
      let go Nothing = Just $ setContactUserState newUserState
          go (Just st) = Just $ setContactUserState st
      alterCache users userId go
      replyText "Please send us a message."

data ForwardMessageType = Feedback | Spam
  deriving (Eq, Show)

forwardToOwnersMaybe :: WithBotState => ForwardMessageType -> ChatId -> MessageId -> BotM ()
forwardToOwnersMaybe fwdType chatId messageId = do
  let BotState {..} = ?model
  let Settings{..} = botSettings
  forM_ ownerGroup $ \OwnerGroupSettings{..} -> do
    let ownerId = SomeChatId $ ChatId ownerGroupId
        threadId = case fwdType of
          Spam -> ownerGroupSpamThreadId
          Feedback -> ownerGroupFeedbackThreadId
        forwardMessageRequest =
          (defForwardMessage ownerId (SomeChatId chatId) messageId)
            { forwardMessageMessageThreadId = Just $ MessageThreadId threadId }
    mResponse <- call (forwardMessage forwardMessageRequest)
    let fwdSent = maybe False responseOk mResponse
    unless fwdSent $ do
      let copyMessageRequest =
            (defCopyMessage ownerId (SomeChatId chatId) messageId)
              { copyMessageMessageThreadId =  Just $ MessageThreadId threadId }
      void $ call $ copyMessage copyMessageRequest
    pure ()          

sendContactMessageAndLeave :: WithBotState => ChatId -> MessageId -> BotM ()
sendContactMessageAndLeave chatId messageId = do
  now <- liftIO getCurrentTime
  sendEvent $ (event now EventLeaveUnsupported)
    { eventChatId = Just chatId }

  sendUnsupportedMessage messageId
  collectFeedback chatId
  forwardToOwnersMaybe Feedback chatId messageId
  void $ call $ leaveChat (SomeChatId chatId)
  pure ()
