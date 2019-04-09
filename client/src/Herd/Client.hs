
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Client
     ( getSubjectIds
     , getSchemaVersions
     , getSchema
     , registerSchema
     , deleteSchema
     -- Subject Log
     , readSubject
     , writeSubject
     , runClient
     ) where

import           Conduit
import           Control.Lens           (Getting, view, (^?))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Avro              (FromAvro, ToAvro)
import qualified Data.Avro              as Avro
import           Data.Avro.Schema       (Schema)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Conduit.Network   (clientSettings)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as T
import           Data.These
import           Data.Time.Clock        (UTCTime)
import           Network.JSONRPC

import           Herd.Protocol
import           Herd.Types

-- data ClientSettings = ClientSettings
--   { _csHost :: Text
--   , _csPort :: Int
--   } deriving (Eq, Show)

-- makeLenses ''ClientSettings

type ClientT m = JSONRPCT m

getSubjectIds :: MonadLoggerIO m => ClientT m [SubjectId]
getSubjectIds = sendToServer GetSubjectIdsReq _GetSubjectIdsRes

getSchemaVersions :: MonadLoggerIO m => SubjectId -> ClientT m (NonEmpty Version)
getSchemaVersions subjectId = sendToServer (GetSchemaVersionsReq subjectId) _GetSchemaVersionsRes

getSchema :: MonadLoggerIO m => SubjectId -> Version -> ClientT m Schema
getSchema subjectId version =
  sendToServer (GetSchemaReq subjectId version) _GetSchemaRes

registerSchema :: MonadLoggerIO m => SubjectId -> Schema -> ClientT m ()
registerSchema subjectId schema =
  sendToServer (RegisterSchemaReq subjectId schema) _RegisterSchemaRes

deleteSchema :: MonadLoggerIO m => SubjectId -> Version -> ClientT m ()
deleteSchema subjectId version =
  sendToServer (DeleteSchemaReq subjectId version) _DeleteSchemaRes

-- Data API

type SubjectRecords a = (These (NonEmpty String) [a])

readSubject :: (MonadLoggerIO m, FromAvro a) => SubjectId -> UTCTime -> ClientT m (SubjectRecords a)
readSubject subjectId from = do
  records <- sendToServer (ReadSubjectReq subjectId from) _ReadSubjectRes
  return $ decodeRecords records

  where decodeRecords :: FromAvro a => [SubjectRecord] -> SubjectRecords a
        decodeRecords []   = That []
        decodeRecords recs = foldr1 (<>) $ fmap (handleAvroResult . Avro.decode . BSL.fromStrict . (view srPayload)) recs

        handleAvroResult :: Avro.Result a -> SubjectRecords a
        handleAvroResult (Avro.Success a) = That [a]
        handleAvroResult (Avro.Error err) = This (err :| [])

writeSubject :: (MonadLoggerIO m, ToAvro a) => SubjectId -> a -> ClientT m SubjectRecordId
writeSubject subjectId payload =
  let binPayload = Avro.encode payload
  in sendToServer (WriteSubjectReq subjectId binPayload) _WriteSubjectRes

-- Manage the actual communication with the server

sendToServer :: (MonadLoggerIO m) => HerdRequest -> Getting (First a) HerdResponse a -> ClientT m a
sendToServer req l = do
  rawRes <- sendRequest req
  res    <- foldResponse rawRes

  case (res ^? l) of
    Nothing -> fail "invalid response"
    Just r  -> return r

  where
    foldResponse :: MonadIO m => Maybe (Either ErrorObj HerdResponse) -> m HerdResponse
    foldResponse Nothing          = fail "could not receive or parse response"
    foldResponse (Just (Left e))  = fail $ fromError e
    foldResponse (Just (Right r)) = return r

-- Run the Herd client monad

runClient :: (MonadUnliftIO m, MonadLoggerIO m) => Text -> Int -> ClientT m a -> m a
runClient host port action = do
  --let jsonrcpSettings = clientSettings (settings ^. csPort) (settings ^. csHost)
  let cs = clientSettings port (T.encodeUtf8 host)
  jsonrpcTCPClient V2 True cs action
