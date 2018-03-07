{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Herd.Storage
     ( StorageProtocol
     --, saveEvent
     , storageProcess
     ) where

import           Control.Distributed.Process
import           Control.Monad               (forever, void)
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.Binary
import           Data.ByteString             (ByteString)
import           Data.Time.Clock             (UTCTime)
import           Data.Typeable
import           GHC.Generics

import           Herd.Storage.Algebra
import           Herd.Storage.Memory
import           Herd.Types

data StorageProtocol =
  SaveEvent SaveEvent'
  deriving (Eq, Show, Generic, Typeable)

instance Binary StorageProtocol

data SaveEvent' = SaveEvent' PersistenceId ByteString UTCTime
  deriving (Eq, Show, Generic, Typeable)

instance Binary SaveEvent'

saveEvent' :: ProcessId -> SaveEvent' -> MemStore Process EventRecord
saveEvent' _ (SaveEvent' persistenceId payload time) =
  saveEvent persistenceId payload time

handleMsg :: ProcessId -> StorageProtocol -> MemStore Process ()
handleMsg pid (SaveEvent msg) = do
  _ <- saveEvent' pid msg
  return ()

storageProcess :: Process ()
storageProcess = do
  pid <- getSelfPid
  evalStateT (loop pid) initialMemStore
  where loop :: ProcessId -> MemStore Process ()
        loop pid = forever $ do
          msg <- lift (expect :: Process StorageProtocol)
          handleMsg pid msg
