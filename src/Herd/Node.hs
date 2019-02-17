{-# LANGUAGE OverloadedStrings #-}

module Herd.Node
     ( startHerd
     ) where

import           Control.Concurrent                                 (threadDelay)
import           Control.Distributed.Process                        hiding
                                                                     (Handler)
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Node                   (LocalNode, initRemoteTable,
                                                                     runProcess)
import           Control.Lens
import           Control.Monad.Morph
import           Control.Monad.State
import qualified Data.ByteString                                    as B
import qualified Data.Text                                          as T
import           Data.Time.Clock
import           Data.Yaml                                          (ParseException,
                                                                     decodeFileEither,
                                                                     prettyPrintParseException)
import qualified Network.Wai.Handler.Warp                           as Wai
import           Servant

import           Herd.API
import           Herd.Config
import           Herd.Storage
import           Herd.Types

parseConfig :: FilePath -> IO (Either ParseException HerdConfig)
parseConfig = decodeFileEither

server :: ProcessId -> Server EventsAPI
--server systemRoot node = fetchEvents
server systemRoot = fetchEvents' systemRoot

--fetchEvents' :: ProcessId -> LocalNode -> Maybe UTCTime -> Handler [EventRecord]
fetchEvents' :: ProcessId -> Handler [EventRecord]
fetchEvents' = undefined
-- fetchEvents' systemRoot node (Just oldest) = execStateT (hoist (\x -> liftIO $ runProcess node x) asyncEvents) []
--   where asyncEvents :: StateT [EventRecord] Process ()
--         asyncEvents = do
--           self <- lift $ getSelfPid
--           lift $ send systemRoot (self, loadRecordsMsg "" oldest)
--           events <- lift $ (expect :: Process [EventRecord])
--           put events

launch :: HerdConfig -> IO ()
launch config = do
  node       <- startNode
  runProcess node $ do
    systemRoot <- launchSystem
    httpServer systemRoot
  where startNode :: IO LocalNode
        startNode = do
          let host = T.unpack $ config ^. hcNetwork . ncCluster . nbHost
          let port = show $ config ^. hcNetwork . ncCluster . nbPort
          backend <- initializeBackend host port initRemoteTable
          newLocalNode backend

        launchSystem :: Process ProcessId
        launchSystem = do
          self       <- getSelfPid
          time1      <- liftIO $ getCurrentTime
          storagePid <- storageProcess $ config ^. hcStorage
          send storagePid (self, saveRecordMsg "foo-entity" B.empty time1)
          liftIO $ threadDelay 10000
          time2      <- liftIO $ getCurrentTime
          send storagePid (self, saveRecordMsg "foo-entity" B.empty time2)
          send storagePid (self, saveRecordMsg "bar-entity" B.empty time1)
          send storagePid (self, loadRecordsMsg "foo-entity" time1)
          response <- (expect :: Process StorageResponse)
          liftIO $ print response
          return self

        httpServer :: ProcessId -> Process ()
        httpServer systemRoot = do
          let httpPort = config ^. hcNetwork . ncHttp . nbPort
          liftIO $ Wai.run httpPort $ serve eventsAPI (server systemRoot)

startHerd :: FilePath -> IO ()
startHerd configFile = do
  decodedConfig <- parseConfig configFile
  case decodedConfig of
    Left  err -> putStrLn $ prettyPrintParseException err
    Right cfg -> launch cfg
