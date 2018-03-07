{-# LANGUAGE OverloadedStrings #-}

module Herd.Node where

import           Control.Concurrent                                 (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Node                   (initRemoteTable,
                                                                     runProcess)
import qualified Data.ByteString                                    as B
import           Data.Time.Clock

import           Herd.Storage

startHerd :: String -> Int -> IO ()
startHerd host port = do
  time    <- getCurrentTime
  backend <- initializeBackend host (show port) initRemoteTable
  node    <- newLocalNode backend
  runProcess node $ do
    storagePid <- storageProcess
    send storagePid (saveRecordMsg "pid" B.empty time)
    liftIO $ threadDelay 2000000
