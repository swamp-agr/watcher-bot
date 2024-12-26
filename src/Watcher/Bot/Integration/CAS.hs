module Watcher.Bot.Integration.CAS where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.Common

import Watcher.Bot.Settings

newCasClient :: IO Manager
newCasClient = newManager tlsManagerSettings

data CasResponse = CasResponse
  { casResponseOk :: Bool
  , casResponseResult :: Maybe CasResult
  , casResponseDescription :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON CasResponse where parseJSON = gparseJSON

data CasResult = CasResult
  { casResultOffenses :: Maybe Int
  , casResultMessages :: [Text]
  , casResultTimeAdded :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON CasResult where parseJSON = gparseJSON

checkUser :: Manager -> CasSettings -> UserId -> IO (Either String CasResponse)
checkUser manager CasSettings{..} (UserId userId) = do
  let request = parseRequest_ $ casEndpoint <> show userId
  response <- httpLbs request manager
  pure $ eitherDecode' @CasResponse $ responseBody response
