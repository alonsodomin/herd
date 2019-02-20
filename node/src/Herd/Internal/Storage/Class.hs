{-# LANGUAGE MultiParamTypeClasses #-}

module Herd.Internal.Storage.Class where

import           Control.Monad.Base
import           Data.ByteString     (ByteString)
import           Data.Time.Clock     (UTCTime)

import           Herd.Internal.Types

class Monad m => MonadStorage m where
  saveRecord :: SubjectId -> ByteString -> UTCTime -> m SubjectRecord
  loadRecords :: SubjectId -> UTCTime -> m [SubjectRecord]

class MonadBase b m => MonadStorage' b m where
  saveRecord2 :: SubjectId -> ByteString -> UTCTime -> m SubjectRecord
  loadRecords2 :: SubjectId -> UTCTime -> m [SubjectRecord]
