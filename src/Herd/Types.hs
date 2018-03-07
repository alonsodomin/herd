{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Herd.Types where

import Control.Lens
import Data.Binary
import Data.Binary.Orphans
import Data.Text (Text)
import Data.Text.Binary
import Data.Typeable
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import GHC.Generics

newtype PersistenceId = PersistenceId Text
  deriving (Eq, Show, Generic, Typeable)

instance Binary PersistenceId

data EventId = EventId PersistenceId Int
  deriving (Eq, Show, Generic, Typeable)

instance Binary EventId

data EventRecord = EventRecord
  { _erEventId :: EventId
  , _erPayload :: ByteString
  , _erTime    :: UTCTime
  } deriving (Eq, Show, Generic, Typeable)

instance Binary EventRecord

makeLenses ''EventRecord