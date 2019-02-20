{-# LANGUAGE OverloadedStrings #-}

module Herd.Node
     ( startHerdNode
     ) where

import           Control.Concurrent                                 (threadDelay)
import           Control.Distributed.Process                        hiding
                                                                     (Handler)
import           Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Control.Distributed.Process.ManagedProcess         as ManagedProcess
import           Control.Distributed.Process.Node                   (LocalNode, initRemoteTable,
                                                                     runProcess)
import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Morph
import           Control.Monad.State
import qualified Data.ByteString                                    as B
import qualified Data.Text                                          as T
import           Data.Time.Clock

import qualified Network.Wai.Handler.Warp                           as Wai
import           Servant

import           Herd.API
import           Herd.Config
import           Herd.Data.Text
import           Herd.Internal.Types
import           Herd.Process.Storage

server :: ProcessId -> LocalNode -> Server EventsAPI
server = fetchEvents'

fetchEvents' :: ProcessId -> LocalNode -> Handler [EventRecord]
fetchEvents' = undefined
-- fetchEvents' systemRoot node = liftIO $ runProcess node $ do
--   self       <- getSelfPid
--   time       <- liftIO $ getCurrentTime
--   send systemRoot (self, loadRecordsMsg "foo-entity" time)
--   response   <- (expect :: Process StorageResponse)
--   return $ getRecords response

startHerdNode :: HerdConfig -> IO ()
startHerdNode config = do
  node       <- startNode
  runProcess node $ do
    systemRoot <- launchSystem
    httpServer systemRoot node
  where startNode :: IO LocalNode
        startNode = do
          let host = config ^. hcNetwork . ncCluster . nbHost
          let port = show $ config ^. hcNetwork . ncCluster . nbPort
          liftIO $ runStdoutLoggingT (logDebugN $ "Starting Herd node at host " <> host <> " and port " <> (toText port) <> "...")
          backend <- initializeBackend (T.unpack host) port initRemoteTable
          newLocalNode backend

        launchSystem :: Process ProcessId
        launchSystem = do
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
          return self

        httpServer :: ProcessId -> LocalNode -> Process ()
        httpServer systemRoot node = do
          let httpPort = config ^. hcNetwork . ncHttp . nbPort
          liftIO $ Wai.run httpPort $ serve eventsAPI (server systemRoot node)
