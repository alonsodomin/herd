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
import           Data.Time.Clock
import           Data.Yaml                                          (ParseException,
                                                                     decodeFileEither,
                                                                     prettyPrintParseException)

import           Herd.Config
import           Herd.Storage

parseConfig :: FilePath -> IO (Either ParseException HerdConfig)
parseConfig = decodeFileEither

launch :: String -> Int -> HerdConfig -> IO ()
launch host port config = do
  time    <- getCurrentTime
  backend <- initializeBackend host (show port) initRemoteTable
  node    <- newLocalNode backend
  runProcess node $ do
    storagePid <- storageProcess $ config ^. hcStorage
    send storagePid (saveRecordMsg "pid" B.empty time)
    send storagePid (saveRecordMsg "pid" B.empty time)
    send storagePid (saveRecordMsg "pid" B.empty time)
    liftIO $ threadDelay 2000000

startHerd :: String -> Int -> FilePath -> IO ()
startHerd host port configFile = do
  decodedConfig <- parseConfig configFile
  case decodedConfig of
    Left  err -> putStrLn $ prettyPrintParseException err
    Right cfg -> launch host port cfg
