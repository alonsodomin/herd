module Herd.Internal.Storage.Class where

import           Control.Monad.Base
import           Data.ByteString     (ByteString)
import           Data.Time.Clock     (UTCTime)

import           Herd.Internal.Types

class Monad m => MonadStorage m where
  saveRecord :: SubjectId -> ByteString -> UTCTime -> m SubjectRecord
  loadRecords :: SubjectId -> UTCTime -> m [SubjectRecord]
