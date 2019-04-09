
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Client
     ( getSubjectIds
     , getSchemaVersions
     , getSchema
     , registerSchema
     , deleteSchema
     , runClient
     ) where

import           Conduit
import           Control.Lens           (Getting, (^?))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
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

type ClientT m = JSONRPCT m

getSubjectIds :: MonadLoggerIO m => ClientT m [SubjectId]
getSubjectIds = sendToServer' GetSubjectIdsReq _GetSubjectIdsRes

getSchemaVersions :: MonadLoggerIO m => SubjectId -> ClientT m (NonEmpty Version)
getSchemaVersions subjectId = sendToServer' (GetSchemaVersionsReq subjectId) _GetSchemaVersionsRes

getSchema :: MonadLoggerIO m => SubjectId -> Version -> ClientT m Schema
getSchema subjectId version =
  sendToServer' (GetSchemaReq subjectId version) _GetSchemaRes

registerSchema :: MonadLoggerIO m => SubjectId -> Schema -> ClientT m ()
registerSchema subjectId schema =
  sendToServer' (RegisterSchemaReq subjectId schema) _RegisterSchemaRes

deleteSchema :: MonadLoggerIO m => SubjectId -> Version -> ClientT m ()
deleteSchema subjectId version =
  sendToServer' (DeleteSchemaReq subjectId version) _DeleteSchemaRes

-- Manage the actual communication with the server

sendToServer :: (MonadLoggerIO m) => HerdRequest -> Getting (First a) HerdResponse a -> (a -> m r) -> ClientT m r
sendToServer req l f = do
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

sendToServer' :: MonadLoggerIO m => HerdRequest -> Getting (First a) HerdResponse a -> ClientT m a
sendToServer' r l = sendToServer r l return

-- Run the Herd client monad

runClient :: (MonadUnliftIO m, MonadLoggerIO m) => Text -> Int -> ClientT m a -> m a
runClient host port action = do
  --let jsonrcpSettings = clientSettings (settings ^. csPort) (settings ^. csHost)
  let cs = clientSettings port (T.encodeUtf8 host)
  jsonrpcTCPClient V2 True cs action
