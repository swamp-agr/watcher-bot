module Telegram.Bot.API.Names where

import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Telegram.Bot.API

import qualified Data.Text as Text

makeName :: (a -> Text) -> (a -> Maybe Text) -> a -> Text
makeName firstNameSelector lastNameSelector v = Text.concat
  [ firstNameSelector v, maybe "" (" " <>) (lastNameSelector v) ]

getUserName :: User -> Text
getUserName = makeName userFirstName userLastName

getChatName :: Chat -> Text
getChatName = makeName (fromMaybe "" . chatFirstName) chatLastName
  
getChatFullInfoName :: ChatFullInfo -> Text
getChatFullInfoName = makeName (fromMaybe "" . chatFullInfoFirstName) chatFullInfoLastName

makeChatLink :: ChatFullInfo -> Text
makeChatLink
  = makeLink "chat" (coerce @_ @Integer . chatFullInfoId)
      chatFullInfoUsername getChatFullInfoName

makeUserLink :: User -> Text
makeUserLink = makeLink "user" (coerce @_ @Integer . userId) userUsername getUserName

makeLink :: Text -> (a -> Integer) -> (a -> Maybe Text) -> (a -> Text) -> a -> Text
makeLink entityType idSelector usernameSelector getName v = case usernameSelector v of
  Nothing -> Text.concat
    [ "<a href=\"tg://", entityType, "?id="
    , Text.pack $ show (idSelector v)
    , "\">"
    , getName v
    , "</a>"
    ]
  Just username -> Text.cons '@' username

normaliseUsername :: Text -> Maybe Text
normaliseUsername txt = case Text.uncons txt of
  Nothing -> Nothing
  Just ('@', _) -> Just txt
  Just (_, _)   -> Just $ Text.cons '@' txt
