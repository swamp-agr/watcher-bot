module Watcher.Bot.Types.UserInfo where

import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Dhall (FromDhall (..), ToDhall (..))
import GHC.Generics (Generic)
import Telegram.Bot.API
import Telegram.Bot.API.Names

import Watcher.Orphans ()

data UserInfo = UserInfo
  { userInfoId :: UserId
  , userInfoFirstName :: Text
  , userInfoLastName :: Maybe Text
  , userInfoLink :: Text
  } deriving (Show, Eq, Generic, FromDhall, ToDhall)

userToUserInfo :: User -> UserInfo
userToUserInfo u@User{..} = UserInfo
  { userInfoId = userId
  , userInfoFirstName = userFirstName
  , userInfoLastName = userLastName
  , userInfoLink = makeUserLink u
  }

chatFullInfoToUserInfo :: ChatFullInfo -> UserInfo
chatFullInfoToUserInfo cf = UserInfo
  { userInfoId = coerce @_ @UserId (chatFullInfoId cf)
  , userInfoFirstName = fromMaybe "" (chatFullInfoFirstName cf)
  , userInfoLastName = chatFullInfoLastName cf
  , userInfoLink = makeLink "user"
      (coerce @_ @Integer . chatFullInfoId) chatFullInfoUsername getChatFullInfoName cf
  }
