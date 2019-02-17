module Herd.Internal.Storage.Class where

import           Data.ByteString     (ByteString)
import           Data.Time.Clock     (UTCTime)

import           Herd.Internal.Types

class Monad m => MonadStorage m where
  saveRecord :: PersistenceId -> ByteString -> UTCTime -> m EventRecord
  loadRecords :: PersistenceId -> UTCTime -> m [EventRecord]
