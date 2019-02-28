{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Internal.Types where

import           Control.Lens                 hiding ((.=))
import           Data.Aeson                   (FromJSON, ToJSON, object,
                                               parseJSON, toJSON, withObject,
                                               (.:), (.:?), (.=))
import qualified Data.Aeson                   as JSON
import           Data.Avro.Schema             (Schema)
import           Data.Binary
import           Data.Binary.Orphans          ()
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base64       as Base64
import           Data.Hashable                (Hashable, hashWithSalt)
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Extra
import           Data.Time.Clock              (UTCTime)
import           Data.Typeable
import           GHC.Generics                 (Generic)
import           Text.ParserCombinators.ReadP (pfail, readP_to_S, readS_to_P)

newtype SubjectId = SubjectId Text
  deriving (Eq, Show, Read, Generic, Typeable, Hashable, Binary, FromJSON, ToJSON)

instance ToText SubjectId where
  toText (SubjectId x) = x

instance IsString SubjectId where
  fromString = SubjectId . T.pack

newtype Version = Version Integer
  deriving (Eq, Show, Read, Ord, Generic, Typeable, Hashable, Binary, FromJSON, ToJSON)

initialVersion :: Version
initialVersion = Version 1

nextVersion :: Version -> Version
nextVersion (Version x) = Version (x + 1)

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
  } deriving (Eq, Show, Read, Binary, Generic, Typeable)

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

newtype AvroSchema = AvroSchema { unwrapSchema :: Schema }
  deriving Eq

instance Show AvroSchema where
  showsPrec p (AvroSchema sch) = showsPrec p (JSON.encode sch)

instance Read AvroSchema where
  readsPrec p = readP_to_S $ AvroSchema <$> do
    bs <- readS_to_P $ readsPrec p
    let decoded = JSON.eitherDecode bs
    case decoded of
      Right x -> return x
      Left _  -> pfail
