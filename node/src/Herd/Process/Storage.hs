{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}

module Herd.Process.Storage
     ( saveRecord
     , loadRecords
     -- Storage server
     , startStorage
     ) where

import           Control.Distributed.Process                hiding (call)
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
import           Data.Text.Extra
import           Data.Time.Clock                            (UTCTime)
import           Data.Typeable
import           GHC.Generics

import           Herd.Config
import           Herd.Internal.Types

-- Protocol definition

data SaveRecord = SaveRecord SubjectId ByteString UTCTime
  deriving (Eq, Show, Generic, Typeable, Binary)

data LoadRecords = LoadRecords SubjectId UTCTime
  deriving (Eq, Show, Generic, Typeable, Binary)

type StoreState = HashMap SubjectId (Integer, NonEmpty SubjectRecord)

-- Client API

saveRecord :: ProcessId -> SubjectId -> ByteString -> UTCTime -> Process SubjectRecord
saveRecord pid subjectId payload time = call pid $ SaveRecord subjectId payload time

loadRecords :: ProcessId -> SubjectId -> UTCTime -> Process [SubjectRecord]
loadRecords pid subjectId oldest = call pid $ LoadRecords subjectId oldest

-- Message handlers

handleSaveRecord :: StoreState -> SaveRecord -> Process (ProcessReply SubjectRecord StoreState)
handleSaveRecord state (SaveRecord subjectId payload time) = do
  (record, newState) <- runStdoutLoggingT $ do
    logDebugN $ "Going to store event record for subject ID '" <> (toText subjectId) <> "'"
    subjectQueue <- pure $ Map.lookup subjectId state
    let newOffset = maybe 0 (+1) $ fst <$> subjectQueue
    let eventId   = SubjectRecordId subjectId newOffset
    let record    = SubjectRecord eventId payload time
    let newQueue  = maybe (record :| []) ((<|) record) $ snd <$> subjectQueue
    let newState  = Map.insert subjectId (newOffset, newQueue) state
    logDebugN $ "Event record for subject ID '" <> (toText subjectId) <> "' successfully stored with id: " <> (toText eventId)
    return (record, newState)
  reply record newState

handleLoadRecords :: StoreState -> LoadRecords -> Process (ProcessReply [SubjectRecord] StoreState)
handleLoadRecords state (LoadRecords subjectId oldest) = do
  subjectData <- pure $ Map.lookup subjectId state
  let recordQueue  = maybe [] NEL.toList $ snd <$> subjectData
  let filteredRecs = takeWhile (\x -> (x ^. erTime) >= oldest) recordQueue
  reply filteredRecs state

-- Storage Server

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
          handleCall handleSaveRecord
        , handleCall handleLoadRecords
        ]
      , unhandledMessagePolicy = Drop
      }

