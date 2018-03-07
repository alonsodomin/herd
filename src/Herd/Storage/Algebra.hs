{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Herd.Storage.Algebra where

import           Control.Monad.Free
import           Control.Monad.Free.TH
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)

import Herd.Types

-- data StorageOp a =
--   SaveEventOp PersistenceId ByteString UTCTime (EventRecord -> a)
--   deriving Functor

-- makeFree ''StorageOp

-- type Storage = Free StorageOp

class Monad m => MonadStorage m where
  saveEvent :: PersistenceId -> ByteString -> UTCTime -> m EventRecord