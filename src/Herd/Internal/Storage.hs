{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Herd.Internal.Storage where

import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.State
import           Data.ByteString      (ByteString)
import           Data.Semigroup       ((<>))
import           Data.Time.Clock      (UTCTime)

import           Herd.Data.Text
import           Herd.Types

class Monad m => MonadStorage m where
  saveRecord :: PersistenceId -> ByteString -> UTCTime -> m EventRecord
  loadRecords :: PersistenceId -> UTCTime -> m [EventRecord]

type StoreS = (Int, [EventRecord])
type MemStore m = LoggingT (StateT StoreS m)

instance (Monad m, MonadLogger m, MonadState StoreS m, MonadIO m) => MonadStorage m where
  saveRecord pid payload time = do
    logDebugN $ "Going to store event record for persistence ID '" <> (toText pid) <> "'"
    (lastSeqNum, prevEvents) <- get
    let newSeqNum = lastSeqNum + 1
    let eventId   = EventId pid newSeqNum
    let record    = EventRecord eventId payload time
    put (newSeqNum, record:prevEvents)
    logDebugN $ "Event record for persistence ID '" <> (toText pid) <> "' successfully stored with id: " <> (toText eventId)
    return record

  loadRecords pid oldest = do
    logDebugN $ "Loading records for persistence ID '" <> (toText pid) <> "'"
    (_, allRecords) <- get
    return $ takeWhile (\x -> (x ^. erTime) >= oldest) $ filter (\x -> (x ^. erPersistenceId) == pid) allRecords

initialMemStore :: StoreS
initialMemStore = (0, [])
