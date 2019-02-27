{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Herd.Internal.Storage.MemStore
     ( StoreState
     , initial
     , MemStore
     , runMemStore
     ) where

import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.State
import           Data.HashMap.Lazy           (HashMap)
import qualified Data.HashMap.Lazy           as Map
import           Data.Semigroup              ((<>))
import           Data.Text.Extra

import           Herd.Internal.Storage.Class
import           Herd.Internal.Types

type StoreState = (Integer, HashMap SubjectId [SubjectRecord])
type MemStore m = LoggingT (StateT StoreState m)

instance (Monad m, MonadLogger m, MonadState StoreState m, MonadIO m) => MonadStorage m where
  saveRecord pid payload time = do
    logDebugN $ "Going to store event record for subject Id '" <> (toText pid) <> "'"
    (lastSeqNum, allRecords) <- get
    let newSeqNum = lastSeqNum + 1
    let eventId   = SubjectRecordId pid newSeqNum
    let record    = SubjectRecord eventId payload time
    newRecords <- pure $ Map.alter (Just . maybe [record] ((:) record)) pid allRecords
    put (newSeqNum, newRecords)
    logDebugN $ "Event record for subject ID '" <> (toText pid) <> "' successfully stored with id: " <> (toText eventId)
    return record

  loadRecords pid oldest = do
    logDebugN $ "Loading records for subject ID '" <> (toText pid) <> "' starting at: " <> (toText oldest)
    (_, allRecords) <- get
    entityRecords <- pure . maybe [] id $ Map.lookup pid allRecords
    foundRecs <- pure $ takeWhile (\x -> (x ^. erTime) >= oldest) $ filter (\x -> (x ^. erSubjectId) == pid) entityRecords
    logDebugN $ "Found " <> (toText $ length foundRecs) <> " records at subject ID '" <> (toText pid) <> "'"
    return foundRecs

initial :: StoreState
initial = (0, Map.empty)

runMemStore :: MonadIO m => MemStore m a -> m a
runMemStore memStore = evalStateT (runStdoutLoggingT memStore) initial
