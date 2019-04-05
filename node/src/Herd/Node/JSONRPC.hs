{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Node.JSONRPC
     ( startJsonRpc
     ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Distributed.Process (Process)
import           Data.Aeson
import qualified Data.Foldable        as F
import           Data.Maybe           (catMaybes)
import qualified Data.Text            as T
import           Data.Conduit.Network   (serverSettings)
import Data.String
import           GHC.Generics
import           Network.JSONRPC

import           Herd.Config
import           Herd.Protocol
import Herd.Node.Core
import Herd.Types
import Herd.Process.SchemaRegistry

fetchSubjectIds' :: HerdNode -> Process [SubjectId]
fetchSubjectIds' node = fetchSubjectIds (node ^. hnSchemaRegistry)

handle :: MonadLoggerIO m => HerdNode -> Respond HerdRequest m HerdResponse
handle herdNode FetchSubjectIds = Right . FetchedSubjectIds <$> invoke fetchSubjectIds' herdNode

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