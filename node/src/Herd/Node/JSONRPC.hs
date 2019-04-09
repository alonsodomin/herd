{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Node.JSONRPC
     ( startRpcServer
     ) where

import           Control.Distributed.Process (Process)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Avro.Schema            (Schema)
import           Data.Conduit.Network        (serverSettings)
import qualified Data.Foldable               as F
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Maybe                  (catMaybes)
import           Data.String
import qualified Data.Text                   as T
import           Network.JSONRPC

import           Herd.Data.Text
import           Herd.Node.Config
import           Herd.Node.Core
import qualified Herd.Process.SchemaRegistry as R
import           Herd.Protocol
import           Herd.Types

-- Adapter methods

getSubjectIds :: HerdNode -> Process [SubjectId]
getSubjectIds node = R.getSubjectIds (node ^. hnSchemaRegistry)

getSchemaVersions :: SubjectId -> HerdNode -> Process (Maybe (NonEmpty Version))
getSchemaVersions subjectId node = R.getVersions (node ^. hnSchemaRegistry) subjectId

getSchema :: SubjectId -> Version -> HerdNode -> Process (Maybe Schema)
getSchema subjectId version node = R.getSchema (node ^. hnSchemaRegistry) subjectId version

registerSchema :: SubjectId -> Schema -> HerdNode -> Process ()
registerSchema sid sch node = R.registerSchema (node ^. hnSchemaRegistry) sid sch

deleteSchema :: SubjectId -> Version -> HerdNode -> Process (Maybe ())
deleteSchema sid v node = R.deleteSchema (node ^. hnSchemaRegistry) sid v

-- Errors

subjectNotFound :: SubjectId -> ErrorObj
subjectNotFound subjectId = ErrorObj {
    getErrMsg  = T.unpack $ "Subject '" <> (toText subjectId) <> "' not found"
  , getErrCode = 101
  , getErrData = toJSON $ SubjectNotFound subjectId
  }

schemaNotFound :: SubjectId -> Version -> ErrorObj
schemaNotFound subjectId version = ErrorObj {
    getErrMsg = T.unpack $ "No schema for subject '" <> (toText subjectId) <> "' and version " <> (toText version)
  , getErrCode = 102
  , getErrData = toJSON $ SchemaNotFound subjectId version
  }

-- JSON-RPC behaviour

type RpcServerT m = HerdActionT (JSONRPCT m)
type RpcHandler m = Respond HerdRequest (HerdActionT m) HerdResponse

handleRpc :: MonadLoggerIO m => RpcHandler m
handleRpc GetSubjectIdsReq = Right . GetSubjectIdsRes <$> invokeAction getSubjectIds
handleRpc (GetSchemaVersionsReq subjectId) = do
  foundVersions <- invokeAction (getSchemaVersions subjectId)
  case foundVersions of
    Nothing -> return . Left $ subjectNotFound subjectId
    Just vs -> do
      $(logDebug) $ "found versions " <> (toText vs) <> " for subject " <> (toText subjectId)
      return . Right $ GetSchemaVersionsRes vs
handleRpc (GetSchemaReq subjectId version) = do
  foundSchema <- invokeAction (getSchema subjectId version)
  case foundSchema of
    Nothing -> return . Left $ schemaNotFound subjectId version
    Just sc -> return . Right $ GetSchemaRes sc
handleRpc (RegisterSchemaReq subjectId schema) = do
  invokeAction (registerSchema subjectId schema)
  return $ Right Done
handleRpc (DeleteSchemaReq subjectId version) = do
  deleted <- invokeAction (deleteSchema subjectId version)
  case deleted of
    Nothing -> return . Left $ schemaNotFound subjectId version
    Just _  -> return $ Right Done

rpcServer :: MonadLoggerIO m => RpcServerT m ()
rpcServer = do
  qM <- lift receiveBatchRequest
  case qM of
    Nothing -> do
      $(logDebug) "closed request channel, exting"
      return ()
    Just (SingleRequest q) -> do
      $(logDebug) "got single request"
      rM <- buildResponse handleRpc q
      F.forM_ rM (lift . sendResponse)
      rpcServer
    Just (BatchRequest qs) -> do
      $(logDebug) "got request batch"
      rs <- catMaybes `liftM` forM qs (buildResponse handleRpc)
      lift . sendBatchResponse $ BatchResponse rs
      rpcServer

startRpcServer :: HerdConfig -> HerdEnv -> IO ()
startRpcServer config env = (env ^. heLogger) $ do
  let host = config ^. hcNetwork . ncBroker . nbHost
  let port = config ^. hcNetwork . ncBroker . nbPort
  let ss   = serverSettings port (fromString . T.unpack $ host)
  $(logInfo) $ "starting Herd RPC server at " <> host <> ":" <> (toText port)
  jsonrpcTCPServer V2 False ss $ runAction env rpcServer