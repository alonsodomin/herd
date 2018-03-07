{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Herd.Storage.Memory
     ( MemStore
     , initialMemStore
     ) where

import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.Semigroup       ((<>))

import           Herd.Data.Text
import           Herd.Storage.Algebra
import           Herd.Types

type MemStore m = LoggingT (StateT (Int, [EventRecord]) m)

instance (Monad m, MonadIO m) => MonadStorage (MemStore m) where
  saveRecord pid payload time = do
    logInfoN $ "Going to store event record for persistence ID '" <> (toText pid) <> "'"
    (lastSeqNum, prevEvents) <- lift $ get
    let newSeqNum = lastSeqNum + 1
    record <- pure $ EventRecord (EventId pid newSeqNum) payload time
    put (newSeqNum, record:prevEvents)
    logInfoN $ "Event record for persistence ID '" <> (toText pid) <> "' successfully stored"
    return record

initialMemStore :: (Int, [EventRecord])
initialMemStore = (0, [])
