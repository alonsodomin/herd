module Herd.Node.API where

import           Data.Avro.Schema            (Schema)
import           Data.ByteString             (ByteString)
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Time.Clock             (UTCTime)

import           Herd.Node.Core
import qualified Herd.Process.SchemaRegistry as Reg
import qualified Herd.Process.SubjectLog     as SLog
import           Herd.Types

-- Internal Registry API

getSubjectIds :: HerdProcess [SubjectId]
getSubjectIds = withRegistry Reg.getSubjectIds

getSchemaVersions :: SubjectId -> HerdProcess (Maybe (NonEmpty Version))
getSchemaVersions subjectId = withRegistry (Reg.getVersions subjectId)

getSchema :: SubjectId -> Version -> HerdProcess (Maybe Schema)
getSchema subjectId version = withRegistry (Reg.getSchema subjectId version)

registerSchema :: SubjectId -> Schema -> HerdProcess ()
registerSchema sid sch = withRegistry (Reg.registerSchema sid sch)

deleteSchema :: SubjectId -> Version -> HerdProcess (Maybe ())
deleteSchema sid v = withRegistry (Reg.deleteSchema sid v)

-- Internal SubjectLog API

readSubject :: SubjectId -> UTCTime -> HerdProcess (Maybe [SubjectRecord])
readSubject sid time = withSubjectLog (SLog.readSubject sid time)

writeSubject :: SubjectId -> ByteString -> UTCTime -> HerdProcess (Maybe SubjectRecordId)
writeSubject sid payload time = withSubjectLog (SLog.writeSubject sid payload time)
