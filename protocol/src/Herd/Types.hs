{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Herd.Types
     ( SubjectId (..)
     , Version
     , initialVersion
     , nextVersion
     , version
     , SubjectRecordId (..)
     , SubjectRecord (..)
     , srSubjectRecordId
     , srSubjectId
     , srPayload
     , srTime
     , AvroSchema (..)
     ) where

import           Control.Lens                 hiding ((.=))
import           Data.Aeson                   (FromJSON, ToJSON, object,
                                               parseJSON, toJSON, withObject,
                                               (.:), (.:?), (.=))
import qualified Data.Aeson                   as JSON
import qualified Data.Aeson.Text              as JSON
import           Data.Avro.Schema             (Schema)
import           Data.Binary                  (Binary (..))
import qualified Data.Binary                  as B
import           Data.Binary.Orphans          ()
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base64       as Base64
import qualified Data.ByteString.Lazy         as BSL
import           Data.Hashable                (Hashable, hashWithSalt)
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time.Clock              (UTCTime)
import           Data.Typeable
import           GHC.Generics                 (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Instances    ()
import           Text.ParserCombinators.ReadP (pfail, readP_to_S, readS_to_P)

import           Herd.Data.Text

newtype SubjectId = SubjectId Text
  deriving (Eq, Show, Read, Generic, Typeable, Hashable, Binary, FromJSON, ToJSON)

instance ToText SubjectId where
  toText (SubjectId x) = x

instance IsString SubjectId where
  fromString = SubjectId . T.pack

instance Arbitrary SubjectId where
  arbitrary = SubjectId <$> arbitrary

newtype Version = Version Integer
  deriving (Eq, Show, Read, Ord, Generic, Typeable, Hashable, Binary, FromJSON, ToJSON)

initialVersion :: Version
initialVersion = Version 1

nextVersion :: Version -> Version
nextVersion (Version x) = Version (x + 1)

version :: Integer -> Maybe Version
version x
  | x >= 1    = Just $ Version x
  | otherwise = Nothing

instance ToText Version where
  toText (Version x) = toText x

instance Arbitrary Version where
  arbitrary = Version <$> suchThat arbitrary (\x -> x >= 1)

data SubjectRecordId = SubjectRecordId SubjectId Integer
  deriving (Eq, Binary, Show, Read, Generic, Typeable, FromJSON, ToJSON)

instance ToText SubjectRecordId where
  toText (SubjectRecordId subjectId seqNum) =
    T.concat [toText subjectId, "#", toText seqNum]

instance Arbitrary SubjectRecordId where
  arbitrary = SubjectRecordId <$> arbitrary <*> arbitrary

data SubjectRecord = SubjectRecord
  { _srSubjectRecordId :: SubjectRecordId
  , _srPayload         :: ByteString
  , _srTime            :: UTCTime
  } deriving (Eq, Show, Read, Binary, Generic, Typeable)

makeLenses ''SubjectRecord

srSubjectId :: Getter SubjectRecord SubjectId
srSubjectId = srSubjectRecordId . (to $ \(SubjectRecordId pid _) -> pid)

instance ToJSON SubjectRecord where
  toJSON record = object
    [ "id"      .= (record ^. srSubjectRecordId)
    , "payload" .= (BS.unpack $ Base64.encode (record ^. srPayload))
    , "time"    .= (record ^. srTime)
    ]

instance FromJSON SubjectRecord where
  parseJSON = withObject "subject-record" $ \o -> do
    _srSubjectRecordId <- o .: "id"
    _srPayload         <- (Base64.encode . BS.pack) <$> o .: "payload"
    _srTime            <- o .: "time"
    return SubjectRecord{..}

instance Arbitrary SubjectRecord where
  arbitrary = SubjectRecord <$> arbitrary <*> arbitrary <*> arbitrary

instance Binary Schema where
  put = put . BSL.toStrict . JSON.encode
  get = (get :: B.Get ByteString) >>= ((either fail pure) . JSON.eitherDecode' . BSL.fromStrict)

newtype AvroSchema = AvroSchema { unwrapSchema :: Schema }
  deriving (Eq, Generic, Typeable, Binary)

instance Show AvroSchema where
  showsPrec p (AvroSchema sch) = showsPrec p (JSON.encode sch)

instance Read AvroSchema where
  readsPrec p = readP_to_S $ AvroSchema <$> do
    bs <- readS_to_P $ readsPrec p
    let decoded = JSON.eitherDecode bs
    case decoded of
      Right x -> return x
      Left _  -> pfail
