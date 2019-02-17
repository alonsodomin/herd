{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Broker
     ( BrokerRequest
     , BrokerResponse
     , startBroker
     ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Conduit.Network
import           Data.Maybe
import           Data.String
import qualified Data.Text            as T
import           GHC.Generics
import           Network.JSONRPC

import           Herd.Config

-- Broker Protocol

data BrokerRequest = BrokerRequest
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance FromRequest BrokerRequest where
  parseParams "req" = Just $ parseJSON
  parseParams _     = Nothing

instance ToRequest BrokerRequest where
  requestMethod BrokerRequest = "req"
  requestIsNotif BrokerRequest = False

data BrokerResponse = BrokerResponse
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance FromResponse BrokerResponse where
  parseResult "req" = Just $ parseJSON
  parseResult _     = Nothing

-- Broker implementation

handleRequest :: MonadLoggerIO m => Respond BrokerRequest m BrokerResponse
handleRequest BrokerRequest = return $ Right BrokerResponse

broker :: MonadLoggerIO m => JSONRPCT m ()
broker = do
  $(logDebug) "Herd broker listening for incoming requests..."
  qM <- receiveBatchRequest
  case qM of
    Nothing -> do
      $(logDebug) "closed request channel, exting"
      return ()
    Just (SingleRequest q) -> do
      $(logDebug) "got request"
      rM <- buildResponse handleRequest q
      forM_ rM sendResponse
      broker
    Just (BatchRequest qs) -> do
      $(logDebug) "got request batch"
      rs <- catMaybes `liftM` forM qs (buildResponse handleRequest)
      sendBatchResponse $ BatchResponse rs
      broker

startBroker :: NetworkBinding -> IO ()
startBroker binding = runStderrLoggingT $ do
  let port = binding ^. nbPort
  let hostPreference = fromString . T.unpack $ binding ^. nbHost
  let settings       = serverSettings port hostPreference
  $(logDebug) "Starting Herd broker..."
  jsonrpcTCPServer V2 False settings broker
