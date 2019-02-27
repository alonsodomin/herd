{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
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
import           Data.Text.Extra
import           Data.Time.Clock        (UTCTime)
import           Data.Typeable
import           GHC.Generics           hiding (to)

newtype SubjectId = SubjectId Text
  deriving (Eq, Show, Read, Generic, Typeable, Hashable, Binary, FromJSON, ToJSON)

instance ToText SubjectId where
  toText (SubjectId x) = x

instance IsString SubjectId where
  fromString = SubjectId . T.pack

newtype Version = Version Integer
  deriving (Eq, Show, Read, Ord, Generic, Typeable, Hashable, Binary, FromJSON, ToJSON)

instance ToText Version where
  toText (Version x) = toText x

data SubjectRecordId = SubjectRecordId SubjectId Integer
  deriving (Eq, Binary, Show, Read, Generic, Typeable, FromJSON, ToJSON)

instance ToText SubjectRecordId where
  toText (SubjectRecordId subjectId seqNum) =
    T.concat [toText subjectId, "#", toText seqNum]

data SubjectRecord = SubjectRecord
  { _erSubjectRecordId :: SubjectRecordId
  , _erPayload         :: ByteString
  , _erTime            :: UTCTime
  } deriving (Eq, Show, Binary, Generic, Typeable)

makeLenses ''SubjectRecord

erSubjectId :: Getter SubjectRecord SubjectId
erSubjectId = erSubjectRecordId . (to $ \(SubjectRecordId pid _) -> pid)

instance ToJSON SubjectRecord where
  toJSON record = object
    [ "id"      .= (record ^. erSubjectRecordId)
    , "payload" .= (BS.unpack $ Base64.encode (record ^. erPayload))
    , "time"    .= (record ^. erTime)
    ]

instance FromJSON SubjectRecord where
  parseJSON = withObject "subject-record" $ \o -> do
    _erSubjectRecordId <- o .: "id"
    _erPayload         <- (Base64.encode . BS.pack) <$> o .: "payload"
    _erTime            <- o .: "time"
    return SubjectRecord{..}
