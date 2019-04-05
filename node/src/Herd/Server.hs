{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Server
     ( startServer
     ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Logger
import           Data.Aeson
import qualified Data.Foldable                    as F
import           Data.Maybe                       (catMaybes)
import qualified Data.Text                        as T
import           GHC.Generics
import           Network.JSONRPC

import           Herd.Config
import           Herd.Internal.Types

data BrokerRequest = FetchSubjectIds
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance FromRequest BrokerRequest where
  parseParams "fetch-subject-ids" = Just . const $ return FetchSubjectIds
  parseParams _                   = Nothing

instance ToRequest BrokerRequest where
  requestMethod FetchSubjectIds = "fetch-subject-ids"

  requestIsNotif = const False

data BrokerResponse = FetchedSubjectIds [SubjectId]
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance FromResponse BrokerResponse where
  parseResult "fetch-subject-ids" = Just parseJSON
  parseResult _                   = Nothing

handle :: MonadLoggerIO m => Respond BrokerRequest m BrokerResponse
handle = undefined

broker :: MonadLoggerIO m => JSONRPCT m ()
broker = do
  $(logDebug) "listening for new request"
  qM <- receiveBatchRequest
  case qM of
    Nothing -> do
      $(logDebug) "closed request channel, exting"
      return ()
    Just (SingleRequest q) -> do
      $(logDebug) "got request"
      rM <- buildResponse handle q
      F.forM_ rM sendResponse
      broker
    Just (BatchRequest qs) -> do
      $(logDebug) "got request batch"
      rs <- catMaybes `liftM` forM qs (buildResponse handle)
      sendBatchResponse $ BatchResponse rs
      broker

startServer :: HerdConfig -> IO ()
startServer config = undefined
