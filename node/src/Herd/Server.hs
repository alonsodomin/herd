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
import qualified Data.Foldable        as F
import           Data.Maybe           (catMaybes)
import qualified Data.Text            as T
import           GHC.Generics
import           Network.JSONRPC

import           Herd.Config
import           Herd.Protocol

handle :: MonadLoggerIO m => Respond HerdRequest m HerdResponse
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
