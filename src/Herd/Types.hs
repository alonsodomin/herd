{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Herd.Types where

import           Control.Lens
import           Data.Binary
import           Data.Binary.Orphans
import           Data.ByteString     (ByteString)
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Binary
import           Data.Time.Clock     (UTCTime)
import           Data.Typeable
import           GHC.Generics

import           Herd.Data.Text

newtype PersistenceId = PersistenceId Text
  deriving (Eq, Show, Generic, Typeable)

instance Binary PersistenceId

instance IsString PersistenceId where
  fromString = PersistenceId . T.pack

instance ToText PersistenceId where
  toText (PersistenceId txt) = txt

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
