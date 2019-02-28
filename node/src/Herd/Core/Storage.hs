module Herd.Core.Storage
     ( herdStorage
     , saveRecord
     ) where

import           Control.Monad.State
import           Control.Monad.Trans
import           Data.ByteString       (ByteString)
import           Data.Time.Clock       (UTCTime)
import           Data.Typeable
import           Transient.Base
import           Transient.Move

import           Herd.Core.Base
import           Herd.Internal.Storage (MemStore, runMemStore)
import qualified Herd.Internal.Storage as Storage
import           Herd.Internal.Types

type StorageBehaviour = MemStore TransIO ()

-- Requests and handlers

data SaveRecord = SaveRecord SubjectId ByteString UTCTime
  deriving (Eq, Show, Read, Typeable)

handleSaveRecord :: StorageBehaviour
handleSaveRecord = do
  (SaveRecord sid payload time) <- lift $ (getMailbox :: TransIO SaveRecord)
  state                         <- get
  (r, newState)                 <- lift $ runMemStore (Storage.saveRecord sid payload time) state
  put newState
  lift $ putMailbox r

-- Storage module definition

herdStorage :: StorageBehaviour
herdStorage = handleSaveRecord

-- Storage API

saveRecord :: SubjectId -> ByteString -> UTCTime -> Dispatch SubjectRecord
saveRecord sid payload time = dispatch (SaveRecord sid payload time)
