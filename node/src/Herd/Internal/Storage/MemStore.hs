{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Herd.Internal.Storage.MemStore
     ( StoreState
     , initial
     , empty
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
--type MemStore m = LoggingT (StateT StoreState m)
type MemStore m = StateT StoreState m

instance (Monad m, MonadState StoreState m, MonadIO m) => MonadStorage m where
  saveRecord sid payload time = do
    --logDebugN $ "Going to store event record for subject Id '" <> (toText sid) <> "'"
    (lastSeqNum, allRecords) <- get
    let newSeqNum = lastSeqNum + 1
    let recordId   = SubjectRecordId sid newSeqNum
    let record    = SubjectRecord recordId payload time
    newRecords <- pure $ Map.alter (Just . maybe [record] ((:) record)) sid allRecords
    put (newSeqNum, newRecords)
    --logDebugN $ "Event record for subject ID '" <> (toText sid) <> "' successfully stored with id: " <> (toText recordId)
    return record

  loadRecords sid oldest = do
    --logDebugN $ "Loading records for subject ID '" <> (toText sid) <> "' starting at: " <> (toText oldest)
    (_, allRecords) <- get
    entityRecords <- pure . maybe [] id $ Map.lookup sid allRecords
    foundRecs <- pure $ takeWhile (\x -> (x ^. erTime) >= oldest) $ filter (\x -> (x ^. erSubjectId) == sid) entityRecords
    --logDebugN $ "Found " <> (toText $ length foundRecs) <> " records at subject ID '" <> (toText sid) <> "'"
    return foundRecs

empty :: StoreState
empty = (0, Map.empty)
{-# INLINE empty #-}

initial = empty
{-# INLINE initial #-}
{-# DEPRECATED initial "Use 'empty' instead" #-}

evalMemStore :: MonadIO m => MemStore m a -> StoreState -> m a
--evalMemStore memStore = evalStateT (runStdoutLoggingT memStore)
evalMemStore memStore = evalStateT memStore

runMemStore :: MonadIO m => MemStore m a -> StoreState -> m (a, StoreState)
--runMemStore memStore = runStateT (runStdoutLoggingT memStore)
runMemStore memStore = runStateT memStore
