
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Client.API
     ( getSubjectIds
     , getSchemaVersions
     , getSchema
     , registerSchema
     , deleteSchema
     -- Subject Log
     , readSubject
     , writeSubject
     ) where

import           Control.Lens         (view)
import           Data.Avro            (FromAvro, ToAvro)
import qualified Data.Avro            as Avro
-- import           Data.Avro.Schema     (Schema)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.List.NonEmpty   (NonEmpty ((:|)))
import           Data.Monoid
import           Data.These
import           Data.Time.Clock      (UTCTime)

import           Herd.Client.Class
import           Herd.Protocol
import           Herd.Types

getSubjectIds :: MonadClient m => m [SubjectId]
getSubjectIds = sendToServer GetSubjectIdsReq _GetSubjectIdsRes

getSchemaVersions :: MonadClient m => SubjectId -> m (NonEmpty Version)
getSchemaVersions subjectId = sendToServer (GetSchemaVersionsReq subjectId) _GetSchemaVersionsRes

getSchema :: MonadClient m => SubjectId -> Version -> m AvroSchema
getSchema subjectId version =
  sendToServer (GetSchemaReq subjectId version) _GetSchemaRes

registerSchema :: MonadClient m => SubjectId -> AvroSchema -> m ()
registerSchema subjectId schema =
  sendToServer (RegisterSchemaReq subjectId schema) _RegisterSchemaRes

deleteSchema :: MonadClient m => SubjectId -> Version -> m ()
deleteSchema subjectId version =
  sendToServer (DeleteSchemaReq subjectId version) _DeleteSchemaRes

-- Data API

type SubjectRecords a = (These (NonEmpty String) [a])

readSubject :: (MonadClient m, FromAvro a) => SubjectId -> UTCTime -> m (SubjectRecords a)
readSubject subjectId from = do
  records <- sendToServer (ReadSubjectReq subjectId from) _ReadSubjectRes
  return $ decodeRecords records

  where decodeRecords :: FromAvro a => [SubjectRecord] -> SubjectRecords a
        decodeRecords []   = That []
        decodeRecords recs = foldr1 (<>) $ fmap (handleAvroResult . Avro.decode . BSL.fromStrict . (view srPayload)) recs

        handleAvroResult :: Avro.Result a -> SubjectRecords a
        handleAvroResult (Avro.Success a) = That [a]
        handleAvroResult (Avro.Error err) = This (err :| [])

writeSubject :: (MonadClient m, ToAvro a) => SubjectId -> a -> m SubjectRecordId
writeSubject subjectId payload =
  let binPayload = Avro.encode payload
  in sendToServer (WriteSubjectReq subjectId binPayload) _WriteSubjectRes
