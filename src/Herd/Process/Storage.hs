{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes                #-}

module Herd.Process.Storage
     ( StorageRequest
     , StorageResponse
     , _SaveRecord
     , _LoadRecords
     , saveRecordMsg
     , loadRecordsMsg
     , getRecords
     , storageProcess
     -- Storage server
     , saveRecordAction
     , startStorage
     ) where

import           Control.Distributed.Process hiding (call)
import           Control.Distributed.Process.Extras.Time
import           Control.Distributed.Process.ManagedProcess
import           Control.Lens                               hiding ((<|))
import           Control.Monad                              (forever)
import           Control.Monad.Logger
import           Control.Monad.State
import           Data.Binary
import           Data.ByteString                            (ByteString)
import           Data.HashMap.Lazy                          (HashMap)
import qualified Data.HashMap.Lazy                          as Map
import           Data.List.NonEmpty                         (NonEmpty (..),
                                                             (<|))
import qualified Data.List.NonEmpty                         as NEL
import qualified Data.Text                                  as T
import           Data.Time.Clock                            (UTCTime)
import           Data.Typeable
import           GHC.Generics

import           Herd.Config
import           Herd.Data.Text
import           Herd.Internal.Storage
import           Herd.Internal.Types

-- Protocol definition

data StorageRequest =
    SaveRecord SaveRecord
  | LoadRecords LoadRecords
  deriving (Eq, Show, Generic, Typeable, Binary)

data SaveRecord = SaveRecord' SubjectId ByteString UTCTime
  deriving (Eq, Show, Generic, Typeable, Binary)

saveRecordMsg :: SubjectId -> ByteString -> UTCTime -> StorageRequest
saveRecordMsg pid payload time =
  SaveRecord $ SaveRecord' pid payload time

data LoadRecords = LoadRecords' SubjectId UTCTime
  deriving (Eq, Show, Generic, Typeable, Binary)

loadRecordsMsg :: SubjectId -> UTCTime -> StorageRequest
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

getRecords :: StorageResponse -> [EventRecord]
getRecords (FoundRecords xs) = xs

type StoreState = HashMap SubjectId (Integer, NonEmpty EventRecord)

-- Message handlers

saveRecord' :: SaveRecord -> MemStore Process ()
saveRecord' (SaveRecord' persistenceId payload time) = do
  saveRecord persistenceId payload time
  return ()

saveRecordAction :: ProcessId -> SubjectId -> ByteString -> UTCTime -> Process EventRecord
saveRecordAction pid subjectId payload time = call pid $ SaveRecord' subjectId payload time

saveRecord'' :: StoreState -> SaveRecord -> Process (ProcessReply EventRecord StoreState)
saveRecord'' state (SaveRecord' subjectId payload time) = do
  (record, newState) <- runStdoutLoggingT $ do
    logDebugN $ "Going to store event record for subject ID '" <> (toText subjectId) <> "'"
    subjectQueue <- pure $ Map.lookup subjectId state
    let newOffset = maybe 0 (+1) $ fst <$> subjectQueue
    let eventId   = EventId subjectId newOffset
    let record    = EventRecord eventId payload time
    let newQueue  = maybe (record :| []) ((<|) record) $ snd <$> subjectQueue
    let newState  = Map.insert subjectId (newOffset, newQueue) state
    logDebugN $ "Event record for subject ID '" <> (toText subjectId) <> "' successfully stored with id: " <> (toText eventId)
    return (record, newState)
  reply record newState

loadRecords' :: ProcessId -> LoadRecords -> MemStore Process ()
loadRecords' sender (LoadRecords' pid oldest) = do
  records <- loadRecords pid oldest
  lift . lift $ send sender (FoundRecords records)

loadRecords'' :: StoreState -> LoadRecords -> Process (ProcessReply StorageResponse StoreState)
loadRecords'' state (LoadRecords' subjectId oldest) = do
  subjectData <- pure $ Map.lookup subjectId state
  let recordQueue  = maybe [] NEL.toList $ snd <$> subjectData
  let filteredRecs = takeWhile (\x -> (x ^. erTime) >= oldest) recordQueue
  reply (FoundRecords filteredRecs) state

-- Process behaviour

startStorage :: StorageConfig -> Process ()
startStorage cfg = serve () (\() -> initStore) storageServer
  where
    initStore :: Process (InitResult StoreState)
    initStore = do
      liftIO $ runStdoutLoggingT (logDebugN $ "Initializing storage from " <> (T.pack $ cfg ^. scDataLocation) <> "...")
      return $ InitOk Map.empty NoDelay

    storageServer :: ProcessDefinition StoreState
    storageServer = defaultProcess
      { apiHandlers = [
          handleCall saveRecord''
        , handleCall loadRecords''
        ]
      , unhandledMessagePolicy = Drop
      }

storageBehaviour :: ProcessId -> (ProcessId, StorageRequest) -> MemStore Process ()
storageBehaviour _ (sender, msg) = case msg of
  SaveRecord  msg' -> saveRecord' msg'
  LoadRecords msg' -> loadRecords' sender msg'

storageProcess :: StorageConfig -> Process ProcessId
storageProcess cfg = spawnLocal $ do
  pid <- getSelfPid
  runMemStore $ do
    logDebugN "Starting storage process"
    loop pid
  where loop :: ProcessId -> MemStore Process ()
        loop self = forever $ do
          msg <- lift $ lift (expect :: Process (ProcessId, StorageRequest))
          storageBehaviour self msg
