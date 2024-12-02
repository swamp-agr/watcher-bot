module Watcher.Bot.Types.Common where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Hashable (Hashable)
import Dhall (FromDhall (..), ToDhall (..))
import Telegram.Bot.API

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16

import Watcher.Orphans ()

newtype MessageText = MessageText Text
  deriving newtype (Eq, Show, Ord, Hashable, FromDhall, ToDhall)

newtype MessageHash = MessageHash ByteString
  deriving newtype (Eq, Show, Ord, FromDhall, ToDhall)

hexSha256 :: Text -> MessageHash
hexSha256 = MessageHash . Base16.encode . SHA256.hash . encodeUtf8

newtype SpamerId = SpamerId UserId
  deriving newtype (Eq, Show, Read, Hashable, FromDhall, ToDhall)

newtype VoterId = VoterId UserId
  deriving newtype (Eq, Show, Hashable, Ord, FromDhall, ToDhall)
