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
import           Herd.Process.SchemaRegistry
import           Herd.Protocol
import           Herd.Types

fetchSubjectIds' :: HerdNode -> Process [SubjectId]
fetchSubjectIds' node = getSubjectIds (node ^. hnSchemaRegistry)

registerSchema' :: SubjectId -> Schema -> HerdNode -> Process ()
registerSchema' subjectId schema node =
  registerSchema (node ^. hnSchemaRegistry) subjectId schema

handle :: MonadLoggerIO m => HerdNode -> Respond HerdRequest m HerdResponse
handle herdNode FetchSubjectIds = Right . FetchedSubjectIds <$> invoke fetchSubjectIds' herdNode
handle herdNode (RegisterSchema subjectId schema) = do
  invoke (registerSchema' subjectId schema) herdNode
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
