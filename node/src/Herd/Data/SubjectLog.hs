module Herd.Data.SubjectLog
     ( SubjectLog
     , empty
     , subjects
     , addRecord
     , getRecords
     ) where

import           Control.Lens
import           Data.ByteString   (ByteString)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.Time.Clock   (UTCTime)

import           Herd.Types

data SubjectLog = SubjectLog (HashMap SubjectId [SubjectRecord]) Integer
  deriving (Eq, Show, Read)

empty :: SubjectLog
empty = SubjectLog Map.empty 0

subjects :: SubjectLog -> [SubjectId]
subjects (SubjectLog index _) = Map.keys index

addRecord :: SubjectId -> ByteString -> UTCTime -> SubjectLog -> (SubjectRecord, SubjectLog)
addRecord subjectId payload time (SubjectLog index lastSeqNum) =
  let newSeqNum = lastSeqNum + 1
      recordId  = SubjectRecordId subjectId newSeqNum
      record    = SubjectRecord recordId payload time
      newIndex  = Map.alter (Just . maybe [record] ((:) record)) subjectId index
      newLog    = SubjectLog newIndex newSeqNum
  in (record, newLog)

getRecords :: SubjectId -> SubjectLog -> [SubjectRecord]
getRecords subjectId (SubjectLog index _) =
  maybe [] id $ Map.lookup subjectId index
