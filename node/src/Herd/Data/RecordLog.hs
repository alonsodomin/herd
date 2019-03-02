module Herd.Data.RecordLog
     ( RecordLog
     , empty
     , subjects
     , addRecord
     , getRecords
     ) where

import           Control.Lens
import           Data.ByteString     (ByteString)
import           Data.HashMap.Lazy   (HashMap)
import qualified Data.HashMap.Lazy   as Map
import           Data.Time.Clock     (UTCTime)

import           Herd.Internal.Types

data RecordLog = RecordLog (HashMap SubjectId [SubjectRecord]) Integer
  deriving (Eq, Show, Read)

empty :: RecordLog
empty = RecordLog Map.empty 0

subjects :: RecordLog -> [SubjectId]
subjects (RecordLog index _) = Map.keys index

addRecord :: SubjectId -> ByteString -> UTCTime -> RecordLog -> (SubjectRecord, RecordLog)
addRecord subjectId payload time (RecordLog index lastSeqNum) =
  let newSeqNum = lastSeqNum + 1
      recordId  = SubjectRecordId subjectId newSeqNum
      record    = SubjectRecord recordId payload time
      newIndex  = Map.alter (Just . maybe [record] ((:) record)) subjectId index
      newLog    = RecordLog newIndex newSeqNum
  in (record, newLog)

getRecords :: SubjectId -> RecordLog -> [SubjectRecord]
getRecords subjectId (RecordLog index _) =
  maybe [] id $ Map.lookup subjectId index
