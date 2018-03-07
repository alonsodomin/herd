{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Herd.Storage
     ( StorageProtocol
     --, saveEvent
     , storageProcess
     ) where

import Control.Distributed.Process
import Control.Monad (forever)
import Control.Monad.State
import Data.Binary
import Data.Typeable
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import GHC.Generics

import Herd.Storage.Algebra
import Herd.Storage.Memory
import Herd.Types

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
handleMsg pid (SaveEvent msg) = saveEvent' pid msg

storageProcess :: Process ()
storageProcess = do
  pid <- getSelfPid
  forever $ do
    msg <- expect :: Process StorageProtocol
    handleMsg (pid, msg)