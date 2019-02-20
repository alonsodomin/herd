{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Herd.Internal.Types where

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

newtype SubjectId = SubjectId Text
  deriving (Eq, Show, Generic, Typeable, Hashable, Binary, ToJSON)

instance IsString SubjectId where
  fromString = SubjectId . T.pack

instance ToText SubjectId where
  toText (SubjectId txt) = txt

newtype Version = Version Integer
  deriving (Eq, Show, Ord, Generic, Typeable, Hashable, Binary)

instance ToText Version where
  toText (Version v) = toText v

data EventId = EventId SubjectId Integer
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

erSubjectId :: Getter EventRecord SubjectId
erSubjectId = erEventId . (to $ \(EventId pid _) -> pid)

instance ToJSON EventRecord where
  toJSON record = object
    [ "eventId" .= (record ^. erEventId)
    , "payload" .= (BS.unpack $ Base64.encode (record ^. erPayload))
    , "time"    .= (record ^. erTime)
    ]

