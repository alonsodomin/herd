{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Herd.Storage.Memory
     ( MemStore(..)
     , initialMemStore
     ) where

-- import Control.Monad.Free (foldFree)
import Control.Monad.State
import Control.Monad.Identity
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)

import Herd.Storage.Algebra
import Herd.Types

type MemStore = StateT (Int, [EventRecord])

instance Monad m => MonadStorage (MemStore m) where
  saveEvent pid payload time = do
    (lastSeqNum, prevEvents) <- get
    let newSeqNum = lastSeqNum + 1
    record <- pure $ EventRecord (EventId pid newSeqNum) payload time
    put (newSeqNum, record:prevEvents)
    return record

initialMemStore :: (Int, [EventRecord])
initialMemStore = (0, [])

-- saveEventOp' :: PersistenceId -> ByteString -> UTCTime -> MemStore EventRecord
-- saveEventOp' pid payload time = do
--   (lastSeqNum, prevEvents) <- get
--   let newSeqNum = lastSeqNum + 1
--   record <- pure $ EventRecord (EventId pid newSeqNum) payload time
--   put (newSeqNum, record:prevEvents)
--   return record

-- memoryStore :: Storage a -> MemStore a
-- memoryStore = foldFree $ \case
--   SaveEventOp pid payload time next ->
--     next <$> saveEventOp' pid payload time