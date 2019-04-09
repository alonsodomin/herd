
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Client
     ( getSubjectIds
     , getSchemaVersions
     , getSchema
     , registerSchema
     , deleteSchema
     , runHerdClient
     ) where

import           Conduit
import           Control.Lens           (Getting, (^?))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Avro              (ToAvro)
import qualified Data.Avro              as Avro
import           Data.Avro.Schema       (Schema)
import qualified Data.ByteString        as BS
import           Data.Conduit.Network   (clientSettings)
import           Data.List.NonEmpty     (NonEmpty)
import           Data.Monoid            (First)
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as T
import           Network.JSONRPC

import           Herd.Protocol
import           Herd.Types

-- data ClientSettings = ClientSettings
--   { _csHost :: Text
--   , _csPort :: Int
--   } deriving (Eq, Show)

-- makeLenses ''ClientSettings

type HerdClientT m = JSONRPCT m

-- Schema management API

getSubjectIds :: MonadLoggerIO m => HerdClientT m [SubjectId]
getSubjectIds = sendToHerd' GetSubjectIdsReq _GetSubjectIdsRes

getSchemaVersions :: MonadLoggerIO m => SubjectId -> HerdClientT m (NonEmpty Version)
getSchemaVersions subjectId = sendToHerd' (GetSchemaVersionsReq subjectId) _GetSchemaVersionsRes

getSchema :: MonadLoggerIO m => SubjectId -> Version -> HerdClientT m Schema
getSchema subjectId version =
  sendToHerd' (GetSchemaReq subjectId version) _GetSchemaRes

registerSchema :: MonadLoggerIO m => SubjectId -> Schema -> HerdClientT m ()
registerSchema subjectId schema =
  sendToHerd' (RegisterSchemaReq subjectId schema) _RegisterSchemaRes

deleteSchema :: MonadLoggerIO m => SubjectId -> Version -> HerdClientT m ()
deleteSchema subjectId version =
  sendToHerd' (DeleteSchemaReq subjectId version) _DeleteSchemaRes

-- Data API

writeSubject :: (MonadLoggerIO m, ToAvro a) => SubjectId -> a -> HerdClientT m SubjectRecordId
writeSubject subjectId payload =
  let binPayload = Avro.encode payload
  in sendToHerd' (WriteSubjectReq subjectId binPayload) _WriteSubjectRes

-- Manage the actual communication with the server

sendToHerd :: (MonadLoggerIO m) => HerdRequest -> Getting (First a) HerdResponse a -> (a -> m r) -> HerdClientT m r
sendToHerd req l f = do
  rawRes <- sendRequest req
  res    <- foldResponse rawRes

  case (res ^? l) of
    Nothing -> fail "invalid response"
    Just r  -> lift $ f r

  where
    foldResponse :: MonadIO m => Maybe (Either ErrorObj HerdResponse) -> m HerdResponse
    foldResponse Nothing          = fail "could not receive or parse response"
    foldResponse (Just (Left e))  = fail $ fromError e
    foldResponse (Just (Right r)) = return r

sendToHerd' :: MonadLoggerIO m => HerdRequest -> Getting (First a) HerdResponse a -> HerdClientT m a
sendToHerd' r l = sendToHerd r l return

-- Run the Herd client monad

runHerdClient :: (MonadUnliftIO m, MonadLoggerIO m) => Text -> Int -> HerdClientT m a -> m a
runHerdClient host port action = do
  --let jsonrcpSettings = clientSettings (settings ^. csPort) (settings ^. csHost)
  let cs = clientSettings port (T.encodeUtf8 host)
  jsonrpcTCPClient V2 True cs action
