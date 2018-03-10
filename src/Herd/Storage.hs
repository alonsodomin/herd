{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}

module Herd.Storage
     ( StorageProtocol
     , _SaveRecord
     , saveRecordMsg
     , storageProcess
     ) where

import           Control.Distributed.Process
import           Control.Lens
import           Control.Monad               (forever)
import           Control.Monad.Logger
import           Control.Monad.State
import           Data.Binary
import           Data.ByteString             (ByteString)
import           Data.Time.Clock             (UTCTime)
import           Data.Typeable
import           GHC.Generics

import           Herd.Config
import           Herd.Internal.Storage
import           Herd.Types

data StorageProtocol =
  SaveRecord SaveRecord
  deriving (Eq, Show, Generic, Typeable)

instance Binary StorageProtocol

_SaveRecord :: Prism' StorageProtocol SaveRecord
_SaveRecord = prism SaveRecord $ \case
  SaveRecord x -> Right x

data SaveRecord = SaveRecord' PersistenceId ByteString UTCTime
  deriving (Eq, Show, Generic, Typeable)

instance Binary SaveRecord

saveRecordMsg :: PersistenceId -> ByteString -> UTCTime -> StorageProtocol
saveRecordMsg pid payload time =
  SaveRecord $ SaveRecord' pid payload time

saveRecord' :: ProcessId -> SaveRecord -> MemStore Process EventRecord
saveRecord' _ (SaveRecord' persistenceId payload time) =
  saveRecord persistenceId payload time

handleMsg :: ProcessId -> StorageProtocol -> MemStore Process ()
handleMsg pid (SaveRecord msg) = do
  _ <- saveRecord' pid msg
  return ()

storageProcess :: StorageConfig -> Process ProcessId
storageProcess cfg = spawnLocal $ do
  pid <- getSelfPid
  evalStateT (runStdoutLoggingT (loop pid)) initialMemStore
  where loop :: ProcessId -> MemStore Process ()
        loop pid = forever $ do
          msg <- lift $ lift (expect :: Process StorageProtocol)
          handleMsg pid msg
