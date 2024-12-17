module Watcher.Bot.Parse where

import Control.Applicative ((<|>))
import Control.Monad (forM, join)
import Data.Coerce (coerce)
import Data.Foldable (asum)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Telegram.Bot.API
import Telegram.Bot.Simple.UpdateParser
import Text.Read (readMaybe)

import qualified Data.Text as Text

import Watcher.Bot.Settings
import Watcher.Bot.Types

-- | How to parse updates from Telegram into actions.
updateToAction :: Settings -> Update -> Maybe Action
updateToAction settings@Settings{..} update
  -- commands
  --   groups 
  | isCommand "spam" update = handleBan settings update
  | isCommand "s" update = handleBan settings update
  | isCommand "undo" update = handleUnban settings update
  | isCommand "u" update = handleUnban settings update

  --   owner's group
  | isCommand "tuning" update = handleTuning settings update
  | isCommand "dump" update = handleDump settings update 

  --   dm
  | isCommand "contact" update = handleContact settings update

  --   both dm and groups
  | isCommand "setup" update = handleSetup settings update
  | isCommand "help" update = handleHelp settings update

  -- callbacks
  | isCallback update = handleCallback settings =<< updateCallbackQuery update

  -- chat member
  | isChatMember update = Just $! Debug update

  -- regular messages and the rest stuff
  | otherwise = handleMessage settings update
  where
    isCallback = isJust . updateCallbackQuery
    isCommand cmd = isJust . parseUpdate (commandWithBotName botName cmd)
    isChatMember = isJust . updateChatMember

  -- | Some user requested bot @/setup@. Let see what we can do about it:
--
-- @/setup@ could come from:
--   - Owner's group
--   - Public group
--   - DM
--   - Unsupported: any channels, private groups.
handleSetup :: Settings -> Update -> Maybe Action
handleSetup settings Update{..}
  | Just msg <- asum [ updateMessage, updateEditedMessage ] =
      case messageSentFrom settings msg of
        OwnerGroup -> Nothing
        DirectMessage userId -> Just $ CheckUserSetup userId (messageMessageId msg)
        PublicGroup groupId userId ->
          Just $ CheckGroupSetup groupId userId $ messageMessageId msg
        PrivateGroup groupId _userId ->
          Just $ SendContactAndQuit groupId $ messageMessageId msg
        Channel groupId -> Just $ SendContactAndQuit groupId $ messageMessageId msg
        Unsupported groupId -> Just $ SendContactAndQuit groupId $ messageMessageId msg
  | otherwise = Nothing

handleTuning :: Settings -> Update -> Maybe Action
handleTuning settings upd@Update{..}
  | Just msg <- asum [ updateMessage, updateEditedMessage ] =
      case messageSentFrom settings msg of
        OwnerGroup -> Just $! Tuning upd
        _ -> Nothing

  | otherwise = Nothing

-- | Some user requsted @/ban@. Let see what we can do about it:
--
-- @/ban@ could come from:
--   - Owner's group.
--   - Public group
--   - DM
--   - Unsupported; any channels, private groups.
handleBan :: Settings -> Update -> Maybe Action
handleBan settings Update{..}
  | Just msg <- asum [ updateMessage, updateEditedMessage ] =
      case messageSentFrom settings msg of
        OwnerGroup -> Nothing
        DirectMessage _userId -> Nothing
        PublicGroup groupId userId -> maybe
          -- prevent flooding chat
          (Just $! DeleteMessage groupId $ messageMessageId msg)
          (Just . BanAction groupId userId (messageMessageId msg))
          (messageReplyToMessage msg)
        PrivateGroup groupId _userId ->
          Just $ SendContactAndQuit groupId $ messageMessageId msg
        Channel groupId -> Just $ SendContactAndQuit groupId $ messageMessageId msg
        Unsupported groupId -> Just $ SendContactAndQuit groupId $ messageMessageId msg
  | otherwise = Nothing

-- | Only admin could request undo someone's ban.
-- For now, bot accepts this command only from the public group.
handleUnban :: Settings -> Update -> Maybe Action
handleUnban settings Update{..}
  | Just msg <- asum [ updateMessage, updateEditedMessage ] =
      case messageSentFrom settings msg of
        OwnerGroup ->
          messageText msg >>= tryParseSomeChatId
          >>= pure . UnbanGlobally (messageMessageId msg)
        DirectMessage _userId -> Nothing
        PublicGroup groupId userId ->
          messageText msg >>= tryParseSomeChatId 
          >>= pure . UnbanAction groupId userId (messageMessageId msg)
        PrivateGroup groupId _userId ->
          Just $ SendContactAndQuit groupId $ messageMessageId msg
        Channel groupId -> Just $ SendContactAndQuit groupId $ messageMessageId msg
        Unsupported groupId -> Just $ SendContactAndQuit groupId $ messageMessageId msg
  | otherwise = Nothing

handleChatMember :: Settings -> ChatMemberUpdated -> Maybe Action
handleChatMember settings cmu@ChatMemberUpdated {..} =
  case chatMemberSentFrom settings cmu of
    OwnerGroup -> Nothing
    DirectMessage _userId -> Nothing
    PublicGroup chatId userId ->
      if chatMemberStatus chatMemberUpdatedNewChatMember `elem` ["banned", "kicked"]
        && userIsBot chatMemberUpdatedFrom
        then Just $! BotBanAction chatId userId chatMemberUpdatedNewChatMember
        else Nothing
    PrivateGroup _chatId _userId -> Nothing
    Channel _groupId -> Nothing
    Unsupported _chatId -> Nothing

-- | @/contact@ could be expected only from DMs.
handleContact :: Settings -> Update -> Maybe Action
handleContact settings Update{..}
  | Just msg <- asum [ updateMessage, updateEditedMessage ] =
      case messageSentFrom settings msg of
        OwnerGroup -> Nothing
        PublicGroup chatId _userId -> Just $! DeleteMessage chatId $ messageMessageId msg
        PrivateGroup chatId _userId ->
          Just $! SendContactAndQuit chatId $ messageMessageId msg
        Channel chatId -> Just $! SendContactAndQuit chatId $ messageMessageId msg
        Unsupported chatId -> Just $! SendContactAndQuit chatId $ messageMessageId msg
        DirectMessage userId -> Just $! ContactOwners userId msg
  | otherwise = Nothing

-- | @/help@ command could be originated either in DM or in group.
-- Output could be different
handleHelp :: Settings -> Update -> Maybe Action
handleHelp settings Update{..}
  | Just msg <- asum [ updateMessage, updateEditedMessage ] =
      case messageSentFrom settings msg of
        OwnerGroup -> Just $! OwnerHelp $! messageMessageId msg
        PublicGroup chatId _userId -> Just $! PublicHelp chatId $! messageMessageId msg
        DirectMessage userId -> Just $! DirectMessageHelp userId $! messageMessageId msg
        PrivateGroup chatId _userId ->
          Just $! SendContactAndQuit chatId $ messageMessageId msg
        Channel chatId -> Just $! SendContactAndQuit chatId $ messageMessageId msg
        Unsupported chatId -> Just $! SendContactAndQuit chatId $ messageMessageId msg
  | otherwise = Nothing

handleDump :: Settings -> Update -> Maybe Action
handleDump settings Update{..}
  | Just msg <- asum [ updateMessage, updateEditedMessage ] =
      case messageSentFrom settings msg of
        OwnerGroup -> Just $! Dump msg
        _ -> Nothing
  | otherwise = Nothing

-- | Bot received update with non-empty callback. It could come
handleCallback :: Settings -> CallbackQuery -> Maybe Action
handleCallback Settings{..} q@CallbackQuery{..} =
  let mMenuId = readMaybe @MenuId =<< fmap Text.unpack callbackQueryData
      mVoteBanId = readMaybe @VoteBanId =<< fmap Text.unpack callbackQueryData
      mAdminBanId = readMaybe @AdminBanId =<< fmap Text.unpack callbackQueryData
      
      fromUserId = userId callbackQueryFrom
      userId' = if debugEnabled then succ fromUserId else fromUserId
      mMessageId = messageMessageId <$> callbackQueryMessage
  in case mMenuId of
    Just menuId -> NavigateTo
        <$> pure (coerce @UserId @ChatId fromUserId)
        <*> mMessageId
        <*> pure menuId

    Nothing -> case mVoteBanId of
      Just voteBanId -> VoteBan
        <$> pure userId'
        <*> mMessageId
        <*> pure voteBanId

      Nothing -> case mAdminBanId of
        Just adminBanId -> AdminBan
          <$> pure userId'
          <*> mMessageId
          <*> pure adminBanId
        Nothing -> Just $! DebugCallback q

-- | In most cases it would be regular textual message.
-- **Note** This is the most frequently used handler.
handleMessage :: Settings -> Update -> Maybe Action
handleMessage settings@Settings{..} upd@Update{..}
  | Just msg <- asum [ updateMessage, updateEditedMessage ] =
      case messageSentFrom settings msg of
        OwnerGroup -> do
          let mFeedbackThreadId = ownerGroupFeedbackThreadId <$> ownerGroup
          join $ forM mFeedbackThreadId $ \feedbackThreadId ->
            if messageMessageThreadId msg == Just (MessageThreadId feedbackThreadId)
            then if isJust $ messageReplyToMessage msg
              then Just $! ContactUser msg
              else Nothing
            else Nothing
        DirectMessage userId  -> Just $! CheckUserContactState userId msg
        PublicGroup chatId userId -> if debugEnabled
          then Just $! Debug upd
          else Just $! Analyse chatId userId msg
        PrivateGroup chatId _userId -> 
          Just $ SendContactAndQuit chatId $ messageMessageId msg
        Channel chatId -> Just $ SendContactAndQuit chatId $ messageMessageId msg
        Unsupported chatId -> Just $ SendContactAndQuit chatId $ messageMessageId msg
  | otherwise = Nothing

tryParseSomeChatId :: Text -> Maybe SomeChatId
tryParseSomeChatId txt
  = parseSomeChatId =<< (listToMaybe . take 1 . drop 1 . Text.words) txt

parseSomeChatId :: Text -> Maybe SomeChatId
parseSomeChatId txt
  = (SomeChatId <$> readMaybe @ChatId (Text.unpack txt)) <|> usernameMaybe txt
  where
    usernameMaybe t = if Just '@' == (fst <$> Text.uncons t)
      then Just (SomeChatUsername t) else Nothing
