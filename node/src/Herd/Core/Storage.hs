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

type StorageBehaviour = TransIO ()

-- Requests and handlers

data SaveRecord = SaveRecord SubjectId ByteString UTCTime
  deriving (Eq, Show, Read, Typeable)

handleSaveRecord :: StorageBehaviour
handleSaveRecord = behaviour $ \(SaveRecord sid payload time) -> do
  state        <- getState
  (r, newSLog) <- pure $ SLog.addRecord sid payload time (state ^. hsSubjectLog)
  setState (hsSubjectLog .~ newSLog $ state)
  return r

data LoadRecords = LoadRecords SubjectId UTCTime
  deriving (Eq, Show, Read, Typeable)

handleLoadRecords :: StorageBehaviour
handleLoadRecords = behaviour $ \(LoadRecords sid oldest) -> do
  state <- getState
  return $ takeWhile (\x -> (x ^. erTime) >= oldest) $ SLog.getRecords sid (state ^. hsSubjectLog)

-- Storage module definition

herdStorage :: StorageBehaviour
herdStorage = handleSaveRecord <|> handleLoadRecords

-- Storage API

saveRecord :: SubjectId -> ByteString -> UTCTime -> Dispatch SubjectRecord
saveRecord sid payload time = dispatch (SaveRecord sid payload time)

loadRecords :: SubjectId -> UTCTime -> Dispatch [SubjectRecord]
loadRecords sid oldest = dispatch (LoadRecords sid oldest)
