{-# LANGUAGE MultiParamTypeClasses #-}

module Herd.Internal.Storage.Class where

import Control.Monad.Base
import           Data.ByteString     (ByteString)
import           Data.Time.Clock     (UTCTime)

import           Herd.Internal.Types

class Monad m => MonadStorage m where
  saveRecord :: PersistenceId -> ByteString -> UTCTime -> m EventRecord
  loadRecords :: PersistenceId -> UTCTime -> m [EventRecord]

class MonadBase b m => MonadStorage' b m where
  saveRecord2 :: PersistenceId -> ByteString -> UTCTime -> m EventRecord
  loadRecords2 :: PersistenceId -> UTCTime -> m [EventRecord]
