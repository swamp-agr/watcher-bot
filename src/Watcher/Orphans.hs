{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Watcher.Orphans where

import Data.Aeson.KeyMap (KeyMap)
import Data.Csv (ToField (..))
import Data.Functor.Contravariant (contramap)
import Data.Hashable (Hashable)
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.Clock.POSIX
import Dhall.Marshal.Decode (FromDhall (..), double, hashMap, strictText)
import Dhall.Marshal.Encode (Encoder (..), ToDhall (..))
import GHC.Generics
import Telegram.Bot.API

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as BS8
import qualified Dhall.Core as Core

deriving newtype instance FromDhall ChatId

deriving newtype instance ToDhall ChatId

deriving newtype instance ToField ChatId

deriving instance Ord ChatId

deriving instance Read ChatId

deriving instance FromDhall ChatType

deriving instance ToDhall ChatType

instance ToDhall Float where
  injectWith _ = Encoder {..}
    where
      embed = Core.DoubleLit . Core.DhallDouble . realToFrac
      declared = Core.Double

instance FromDhall Float where
  autoWith _ = realToFrac <$> double

instance FromDhall v => FromDhall (KeyMap v) where
  autoWith opts = keyMap (autoWith opts)
    where
      keyMap = fmap KeyMap.fromHashMapText . hashMap strictText

instance ToDhall v => ToDhall (KeyMap v) where
  injectWith opts = contramap KeyMap.toHashMapText (injectWith opts)

deriving newtype instance FromDhall MessageId

deriving newtype instance ToDhall MessageId

deriving newtype instance FromDhall MessageThreadId

deriving newtype instance ToDhall MessageThreadId

instance ToDhall POSIXTime where
  injectWith _ = Encoder {..}
    where
      embed = Core.DoubleLit . Core.DhallDouble . realToFrac
      declared = Core.Double

instance FromDhall POSIXTime where
  autoWith _ = realToFrac <$> double

deriving instance Read MessageId

deriving newtype instance FromDhall Seconds

deriving newtype instance ToDhall Seconds

deriving instance Eq User

deriving newtype instance Enum UserId

deriving newtype instance Ord UserId

deriving newtype instance Read UserId

deriving newtype instance FromDhall UserId

deriving newtype instance ToDhall UserId

deriving newtype instance ToField UserId

deriving instance Eq ChatType

deriving newtype instance Hashable UserId

deriving instance Generic UserId

instance ToField UTCTime where
  toField = BS8.pack . iso8601Show
