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
  , userInfoUsername :: Maybe Text
  , userInfoLink :: Text
  } deriving (Show, Eq, Generic, FromDhall, ToDhall)

makeUserInfo :: UserId -> UserInfo
makeUserInfo uid = UserInfo
  { userInfoId = uid
  , userInfoFirstName = ""
  , userInfoLastName = Nothing
  , userInfoUsername = Nothing
  , userInfoLink = makeLink "user" (coerce @UserId @Integer) (const Nothing) (const "N/A") uid
  }

userToUserInfo :: User -> UserInfo
userToUserInfo u@User{..} = UserInfo
  { userInfoId = userId
  , userInfoFirstName = userFirstName
  , userInfoLastName = userLastName
  , userInfoLink = makeUserLink u
  , userInfoUsername = userUsername
  }

chatFullInfoToUserInfo :: ChatFullInfo -> UserInfo
chatFullInfoToUserInfo cf = UserInfo
  { userInfoId = coerce @_ @UserId (chatFullInfoId cf)
  , userInfoFirstName = fromMaybe "" (chatFullInfoFirstName cf)
  , userInfoLastName = chatFullInfoLastName cf
  , userInfoLink = makeLink "user"
      (coerce @_ @Integer . chatFullInfoId) chatFullInfoUsername getChatFullInfoName cf
  , userInfoUsername = chatFullInfoUsername cf
  }

class ToUserInfo a where
  toUserInfo :: a -> UserInfo
  toUserId :: a -> UserId

instance ToUserInfo User where
  toUserInfo = userToUserInfo
  toUserId = userId

instance ToUserInfo ChatFullInfo where
  toUserInfo = chatFullInfoToUserInfo
  toUserId = coerce @_ @UserId . chatFullInfoId

instance ToUserInfo UserInfo where
  toUserInfo = id
  toUserId = userInfoId

instance ToUserInfo UserId where
  toUserInfo = makeUserInfo
  toUserId = id
