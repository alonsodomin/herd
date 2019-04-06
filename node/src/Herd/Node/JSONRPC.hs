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
import           GHC.Generics
import           Network.JSONRPC

import           Herd.Config
import           Herd.Node.Core
import qualified Herd.Process.SchemaRegistry as R
import           Herd.Protocol
import           Herd.Types

-- Adapter methods

getSubjectIds :: HerdNode -> Process [SubjectId]
getSubjectIds node = R.getSubjectIds (node ^. hnSchemaRegistry)

getSchemaVersions :: SubjectId -> HerdNode -> Process (Maybe [Version])
getSchemaVersions subjectId node = R.getVersions (node ^. hnSchemaRegistry) subjectId

registerSchema :: SubjectId -> Schema -> HerdNode -> Process ()
registerSchema sid sch node = R.registerSchema (node ^. hnSchemaRegistry) sid sch

subjectNotFound :: SubjectId -> ErrorObj
subjectNotFound subjectId@(SubjectId sid) = ErrorObj {
    getErrMsg  = "Subject '" ++ (T.unpack sid) ++ "' not found."
  , getErrCode = 101
  , getErrData = toJSON $ SubjectNotFound subjectId
  }

-- JSON-RPC behaviour

handle :: MonadLoggerIO m => HerdNode -> Respond HerdRequest m HerdResponse
handle herdNode GetSubjectIds = Right . SubjectIds <$> invoke getSubjectIds herdNode
handle herdNode (GetSchemaVersions subjectId) = do
  foundVersions <- invoke (getSchemaVersions subjectId) herdNode
  return $ case foundVersions of
    Nothing -> Left $ subjectNotFound subjectId
    Just vs -> Right $ SchemaVersions vs
handle herdNode (RegisterSchema subjectId schema) = do
  invoke (registerSchema subjectId schema) herdNode
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
      rM <- buildResponse (handle herdNode) q
      F.forM_ rM sendResponse
      broker herdNode
    Just (BatchRequest qs) -> do
      $(logDebug) "got request batch"
      rs <- catMaybes `liftM` forM qs (buildResponse (handle herdNode))
      sendBatchResponse $ BatchResponse rs
      broker herdNode

startJsonRpc :: HerdConfig -> HerdNode -> IO ()
startJsonRpc config herdNode = runStderrLoggingT $ do
  let host = fromString . T.unpack $ config ^. hcNetwork . ncBroker . nbHost
  let port = config ^. hcNetwork . ncBroker . nbPort
  let ss   = serverSettings port host
  jsonrpcTCPServer V2 False ss $ broker herdNode
