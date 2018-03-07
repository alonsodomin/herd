module Herd.Node where

import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Node                   (initRemoteTable,
                                                                     runProcess)

import           Herd.Storage

startHerd :: String -> Int -> IO ()
startHerd host port = do
  backend <- initializeBackend host (show port) initRemoteTable
  node    <- newLocalNode backend
  runProcess node storageProcess
