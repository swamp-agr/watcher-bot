module Watcher.Bot.Types.Common where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Hashable (Hashable)
import Data.Vector.Hashtables
import Dhall (FromDhall (..), ToDhall (..))
import Telegram.Bot.API

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Hashtables as HT
import qualified Data.Vector.Mutable as VM

import Watcher.Orphans ()

data MessageCheck = MessageCheck | MessageCheckWithForward
  deriving (Eq, Show)

newtype MessageText = MessageText Text
  deriving newtype (Eq, Show, Ord, Hashable, FromDhall, ToDhall)

newtype MessageHash = MessageHash ByteString
  deriving newtype (Eq, Show, Ord, FromDhall, ToDhall)

hexSha256 :: Text -> MessageHash
hexSha256 = MessageHash . Base16.encode . SHA256.hash . encodeUtf8

data BanRequestedBy
  = ByBot UserId | ByUser VoterId | BySpamer SpamerId | ByAdmin UserId
  deriving (Eq, Show)

toVoterId :: BanRequestedBy -> VoterId
toVoterId = \case
  ByBot userId -> VoterId userId
  ByUser voterId -> voterId
  BySpamer spamerId -> coerce spamerId
  ByAdmin userId -> VoterId userId

newtype SpamerId = SpamerId UserId
  deriving newtype (Eq, Show, Read, Hashable, FromDhall, ToDhall)

newtype VoterId = VoterId UserId
  deriving newtype (Eq, Show, Hashable, Ord, FromDhall, ToDhall)

type HashMap k v = Dictionary (VM.PrimState IO) VM.MVector k VM.MVector v

toHMap :: Hashable k => HM.HashMap k v -> IO (HashMap k v)
toHMap = HT.fromList . HM.toList

fromHMap :: Hashable k => HashMap k v -> IO (HM.HashMap k v)
fromHMap = fmap HM.fromList . HT.toList

type HashSet k =  HashMap k ()

toHSet :: Hashable k => HS.HashSet k -> IO (HashSet k)
toHSet = HT.fromList . fmap (, ()) . HS.toList

fromHSet :: Hashable k => HashSet k -> IO (HS.HashSet k)
fromHSet = fmap (HS.fromList . fmap fst) . HT.toList
