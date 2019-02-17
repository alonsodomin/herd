{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Herd.Storage
     ( StorageRequest
     , StorageResponse
     , _SaveRecord
     , _LoadRecords
     , saveRecordMsg
     , loadRecordsMsg
     , storageProcess
     ) where

import           Control.Distributed.Process
import           Control.Lens
import           Control.Monad               (forever)
import           Control.Monad.Logger
import           Control.Monad.State
import           Data.Binary
import           Data.ByteString             (ByteString)
import           Data.Time.Clock             (UTCTime)
import           Data.Typeable
import           GHC.Generics

import           Herd.Config
import           Herd.Internal.Storage
import           Herd.Types

-- Protocol definition

data StorageRequest =
    SaveRecord SaveRecord
  | LoadRecords LoadRecords
  deriving (Eq, Show, Generic, Typeable, Binary)

data SaveRecord = SaveRecord' PersistenceId ByteString UTCTime
  deriving (Eq, Show, Generic, Typeable, Binary)

saveRecordMsg :: PersistenceId -> ByteString -> UTCTime -> StorageRequest
saveRecordMsg pid payload time =
  SaveRecord $ SaveRecord' pid payload time

data LoadRecords = LoadRecords' PersistenceId UTCTime
  deriving (Eq, Show, Generic, Typeable, Binary)

loadRecordsMsg :: PersistenceId -> UTCTime -> StorageRequest
loadRecordsMsg pid oldest = LoadRecords $ LoadRecords' pid oldest

_SaveRecord :: Prism' StorageRequest SaveRecord
_SaveRecord = prism' SaveRecord $ \case
  SaveRecord x -> Just x
  _            -> Nothing

_LoadRecords :: Prism' StorageRequest LoadRecords
_LoadRecords = prism' LoadRecords $ \case
  LoadRecords x -> Just x
  _             -> Nothing

data StorageResponse = FoundRecords [EventRecord]
  deriving (Eq, Show, Generic, Typeable, Binary)

-- Message handlers

runInMemory :: MemStore Process a -> Process a
runInMemory memStore = evalStateT (runStdoutLoggingT memStore) initialMemStore

saveRecord' :: SaveRecord -> MemStore Process ()
saveRecord' (SaveRecord' persistenceId payload time) = do
  saveRecord persistenceId payload time
  return ()

loadRecords' :: ProcessId -> LoadRecords -> MemStore Process ()
loadRecords' sender (LoadRecords' pid oldest) = do
  records <- loadRecords pid oldest
  lift . lift $ send sender (FoundRecords records)

-- Process behaviour

storageBehaviour :: ProcessId -> (ProcessId, StorageRequest) -> MemStore Process ()
storageBehaviour _ (sender, msg) = case msg of
  SaveRecord  msg' -> saveRecord' msg'
  LoadRecords msg' -> loadRecords' sender msg'

storageProcess :: StorageConfig -> Process ProcessId
storageProcess cfg = spawnLocal $ do
  pid <- getSelfPid
  runInMemory $ do
    logDebugN "Starting storage process"
    loop pid
  where loop :: ProcessId -> MemStore Process ()
        loop self = forever $ do
          msg <- lift $ lift (expect :: Process (ProcessId, StorageRequest))
          storageBehaviour self msg
