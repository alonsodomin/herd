module Herd.Core.Storage
     ( herdStorage
     , saveRecord
     , loadRecords
     ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.ByteString      (ByteString)
import           Data.Time.Clock      (UTCTime)
import           Data.Typeable
import           Transient.Base
import           Transient.Move

import           Herd.Core.Base
import           Herd.Data.SubjectLog (SubjectLog)
import qualified Herd.Data.SubjectLog as SLog
import           Herd.Internal.Types

type StorageBehaviour = StateT SubjectLog TransIO ()

-- Requests and handlers

data SaveRecord = SaveRecord SubjectId ByteString UTCTime
  deriving (Eq, Show, Read, Typeable)

handleSaveRecord :: StorageBehaviour
handleSaveRecord = behaviour $ \(SaveRecord sid payload time) -> do
  slog         <- get
  (r, newSlog) <- pure $ SLog.addRecord sid payload time slog
  put newSlog
  return r

data LoadRecords = LoadRecords SubjectId UTCTime
  deriving (Eq, Show, Read, Typeable)

handleLoadRecords :: StorageBehaviour
handleLoadRecords = behaviour $ \(LoadRecords sid oldest) -> do
  slog <- get
  return $ takeWhile (\x -> (x ^. srTime) >= oldest) $ SLog.getRecords sid slog

-- Storage module definition

herdStorage :: StorageBehaviour
herdStorage = handleSaveRecord <|> handleLoadRecords

-- Storage API

saveRecord :: SubjectId -> ByteString -> UTCTime -> Dispatch SubjectRecord
saveRecord sid payload time = dispatch (SaveRecord sid payload time)

loadRecords :: SubjectId -> UTCTime -> Dispatch [SubjectRecord]
loadRecords sid oldest = dispatch (LoadRecords sid oldest)
