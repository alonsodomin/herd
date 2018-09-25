{-# LANGUAGE OverloadedStrings #-}

module Herd.Node
     ( startHerd
     ) where

import           Control.Concurrent                                 (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Node                   (initRemoteTable,
                                                                     runProcess)
import           Control.Lens
import qualified Data.ByteString                                    as B
import qualified Data.Text                                          as T
import           Data.Time.Clock
import           Data.Yaml                                          (ParseException,
                                                                     decodeFileEither,
                                                                     prettyPrintParseException)

import           Herd.Config
import           Herd.Storage
import           Herd.Types

parseConfig :: FilePath -> IO (Either ParseException HerdConfig)
parseConfig = decodeFileEither

launch :: HerdConfig -> IO ()
launch config = do
  time    <- getCurrentTime
  let host = T.unpack $ config ^. hcNetwork . ncHost
  let port = show $ config ^. hcNetwork . ncPort
  backend <- initializeBackend host port initRemoteTable
  node    <- newLocalNode backend
  runProcess node $ do
    self       <- getSelfPid
    storagePid <- storageProcess $ config ^. hcStorage
    send storagePid (self, saveRecordMsg "foo-entity" B.empty time)
    send storagePid (self, saveRecordMsg "foo-entity" B.empty time)
    send storagePid (self, saveRecordMsg "bar-entity" B.empty time)
    send storagePid (self, loadRecordsMsg "foo-entity" time)
    records <- (expect :: Process [EventRecord])
    liftIO $ print records
    liftIO $ threadDelay 2000000

startHerd :: FilePath -> IO ()
startHerd configFile = do
  decodedConfig <- parseConfig configFile
  case decodedConfig of
    Left  err -> putStrLn $ prettyPrintParseException err
    Right cfg -> launch cfg
