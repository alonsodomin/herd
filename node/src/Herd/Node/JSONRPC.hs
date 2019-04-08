{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Node.JSONRPC
     ( startJsonRpc
     ) where

import           Control.Distributed.Process (Process)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Avro.Schema            (Schema)
import           Data.Conduit.Network        (serverSettings)
import qualified Data.Foldable               as F
import           Data.Maybe                  (catMaybes)
import           Data.String
import qualified Data.Text                   as T
import           Network.JSONRPC

import           Herd.Config
import           Herd.Data.Text
import           Herd.Node.Core
import qualified Herd.Process.SchemaRegistry as R
import           Herd.Protocol
import           Herd.Types

-- Adapter methods

getSubjectIds :: HerdNode -> Process [SubjectId]
getSubjectIds node = R.getSubjectIds (node ^. hnSchemaRegistry)

getSchemaVersions :: SubjectId -> HerdNode -> Process (Maybe [Version])
getSchemaVersions subjectId node = R.getVersions (node ^. hnSchemaRegistry) subjectId

getSchema :: SubjectId -> Version -> HerdNode -> Process (Maybe Schema)
getSchema subjectId version node = R.getSchema (node ^. hnSchemaRegistry) subjectId version

registerSchema :: SubjectId -> Schema -> HerdNode -> Process ()
registerSchema sid sch node = R.registerSchema (node ^. hnSchemaRegistry) sid sch

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

handleRpc :: MonadLoggerIO m => HerdNode -> Respond HerdRequest m HerdResponse
handleRpc herdNode GetSubjectIdsReq = Right . GetSubjectIdsRes <$> invokeHerd getSubjectIds herdNode
handleRpc herdNode (GetSchemaVersionsReq subjectId) = do
  foundVersions <- invokeHerd (getSchemaVersions subjectId) herdNode
  case foundVersions of
    Nothing -> return . Left $ subjectNotFound subjectId
    Just vs -> do
      $(logDebug) $ "found versions " <> (toText vs) <> " for subject " <> (toText subjectId)
      return . Right $ GetSchemaVersionsRes vs
handleRpc herdNode (GetSchemaReq subjectId version) = do
  foundSchema <- invokeHerd (getSchema subjectId version) herdNode
  case foundSchema of
    Nothing -> return . Left $ schemaNotFound subjectId version
    Just sc -> return . Right $ GetSchemaRes sc
handleRpc herdNode (RegisterSchemaReq subjectId schema) = do
  invokeHerd (registerSchema subjectId schema) herdNode
  return $ Right Done

broker :: MonadLoggerIO m => HerdNode -> JSONRPCT m ()
broker herdNode = do
  $(logDebug) "listening for new request"
  qM <- receiveBatchRequest
  case qM of
    Nothing -> do
      $(logDebug) "closed request channel, exting"
      return ()
    Just (SingleRequest q) -> do
      $(logDebug) "got request"
      rM <- buildResponse (handleRpc herdNode) q
      F.forM_ rM sendResponse
      broker herdNode
    Just (BatchRequest qs) -> do
      $(logDebug) "got request batch"
      rs <- catMaybes `liftM` forM qs (buildResponse (handleRpc herdNode))
      sendBatchResponse $ BatchResponse rs
      broker herdNode

startJsonRpc :: HerdConfig -> HerdNode -> IO ()
startJsonRpc config herdNode = runStderrLoggingT $ do
  let host = fromString . T.unpack $ config ^. hcNetwork . ncBroker . nbHost
  let port = config ^. hcNetwork . ncBroker . nbPort
  let ss   = serverSettings port host
  jsonrpcTCPServer V2 False ss $ broker herdNode
