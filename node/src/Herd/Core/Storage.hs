module Herd.Core.Storage
     ( herdStorage
     , saveRecord
     , loadRecords
     ) where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.ByteString       (ByteString)
import           Data.Time.Clock       (UTCTime)
import           Data.Typeable
import           Transient.Base
import           Transient.Move

import           Herd.Core.Base
import           Herd.Internal.Storage (MemStore, evalMemStore, runMemStore)
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

data LoadRecords = LoadRecords SubjectId UTCTime
  deriving (Eq, Show, Read, Typeable)

handleLoadRecords :: StorageBehaviour
handleLoadRecords = do
  (LoadRecords sid oldest) <- lift $ (getMailbox :: TransIO LoadRecords)
  state                    <- get
  records                  <- lift $ evalMemStore (Storage.loadRecords sid oldest) state
  lift $ putMailbox records

-- Storage module definition

herdStorage :: StorageBehaviour
herdStorage = handleSaveRecord <|> handleLoadRecords

-- Storage API

saveRecord :: SubjectId -> ByteString -> UTCTime -> Dispatch SubjectRecord
saveRecord sid payload time = dispatch (SaveRecord sid payload time)

loadRecords :: SubjectId -> UTCTime -> Dispatch [SubjectRecord]
loadRecords sid oldest = dispatch (LoadRecords sid oldest)
