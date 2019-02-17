{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Herd.Types where

import           Control.Lens           hiding ((.=))
import           Data.Aeson
import           Data.Binary
import           Data.Binary.Orphans    ()
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as Base64
import           Data.Hashable          (Hashable)
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time.Clock        (UTCTime)
import           Data.Typeable
import           GHC.Generics           hiding (to)

import           Herd.Data.Text

newtype PersistenceId = PersistenceId Text
  deriving (Eq, Show, Generic, Typeable, Hashable, Binary, ToJSON)

instance IsString PersistenceId where
  fromString = PersistenceId . T.pack

instance ToText PersistenceId where
  toText (PersistenceId txt) = txt

data EventId = EventId PersistenceId Int
  deriving (Eq, Binary, Show, Generic, Typeable, ToJSON)

instance ToText EventId where
  toText (EventId persistenceId seqNum) =
    T.concat [toText persistenceId, "#", toText seqNum]

data EventRecord = EventRecord
  { _erEventId :: EventId
  , _erPayload :: ByteString
  , _erTime    :: UTCTime
  } deriving (Eq, Show, Binary, Generic, Typeable)

makeLenses ''EventRecord

erPersistenceId :: Getter EventRecord PersistenceId
erPersistenceId = erEventId . (to $ \(EventId pid _) -> pid)

instance ToJSON EventRecord where
  toJSON record = object
    [ "eventId" .= (record ^. erEventId)
    , "payload" .= (BS.unpack $ Base64.encode (record ^. erPayload))
    , "time"    .= (record ^. erTime)
    ]

