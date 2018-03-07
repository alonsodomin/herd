{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Herd.Internal.Storage where

import           Control.Monad.Logger
import           Control.Monad.State
import           Data.ByteString      (ByteString)
import           Data.Semigroup       ((<>))
import           Data.Time.Clock      (UTCTime)

import           Herd.Data.Text
import           Herd.Types

class Monad m => MonadStorage m where
  saveRecord :: PersistenceId -> ByteString -> UTCTime -> m EventRecord

type MemStoreS = (Int, [EventRecord])

type MemStore m = LoggingT (StateT MemStoreS m)

instance (Monad m, MonadIO m) => MonadStorage (MemStore m) where
  saveRecord pid payload time = do
    logDebugN $ "Going to store event record for persistence ID '" <> (toText pid) <> "'"
    (lastSeqNum, prevEvents) <- lift $ get
    let newSeqNum = lastSeqNum + 1
    let eventId   = EventId pid newSeqNum
    let record    = EventRecord eventId payload time
    put (newSeqNum, record:prevEvents)
    logDebugN $ "Event record for persistence ID '" <> (toText pid) <> "' successfully stored with id: " <> (toText eventId)
    return record

initialMemStore :: MemStoreS
initialMemStore = (0, [])
