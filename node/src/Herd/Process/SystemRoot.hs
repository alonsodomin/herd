{-# LANGUAGE OverloadedStrings #-}

module Herd.Process.SystemRoot where

import           Control.Concurrent                                 (threadDelay)
import           Control.Distributed.Process                        hiding
                                                                     (Handler)
import           Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Control.Distributed.Process.ManagedProcess         as ManagedProcess
import           Control.Distributed.Process.Node                   (LocalNode, initRemoteTable,
                                                                     runProcess)
import           Control.Lens
import           Control.Monad.Logger
import qualified Data.ByteString                                    as B
import qualified Data.Text                                          as T
import           Data.Text.Extra
import           Data.Time.Clock

import           Herd.Config
import           Herd.Process.Storage

startLocalNode :: HerdConfig -> IO ()
startLocalNode config = do
  node       <- startNode
  runProcess node rootSupervisor
  where startNode :: IO LocalNode
        startNode = do
          let host = config ^. hcNetwork . ncCluster . nbHost
          let port = show $ config ^. hcNetwork . ncCluster . nbPort
          liftIO $ runStdoutLoggingT (logDebugN $ "Starting Herd node at host " <> host <> " and port " <> (toText port) <> "...")
          backend <- initializeBackend (T.unpack host) port initRemoteTable
          newLocalNode backend

        rootSupervisor :: Process ()
        rootSupervisor = do
          self       <- getSelfPid
          time1      <- liftIO $ getCurrentTime
          storagePid <- spawnLocal $ startStorage (config ^. hcStorage)
          _ <- saveRecord storagePid "foo-entity" B.empty time1
          liftIO $ threadDelay 10000
          time2      <- liftIO $ getCurrentTime
          _ <- saveRecord storagePid "foo-entity" B.empty time2
          _ <- saveRecord storagePid "bar-entity" B.empty time1
          fooRecs <- loadRecords storagePid "foo-entity" time1
          liftIO $ print fooRecs
