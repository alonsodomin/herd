{-# LANGUAGE DeriveAnyClass #-}

module Herd.Node.Process.SubjectLog
     ( SubjectLogServer
     , writeSubject
     , spawnSubjectLogServer
     ) where

import           Control.Distributed.Process                (Process, ProcessId,
                                                             spawnLocal)
import           Control.Distributed.Process.Extras         hiding (sendChan)
import           Control.Distributed.Process.Extras.Time    (Delay (..))
import           Control.Distributed.Process.ManagedProcess
import           Data.Binary
import           Data.ByteString                            (ByteString)
import           Data.Time.Clock                            (UTCTime)
import           Data.Typeable
import           GHC.Generics

import           Herd.Data.SubjectLog                       (SubjectLog)
import qualified Herd.Data.SubjectLog                       as SLog
import           Herd.Types

-- Protocol definition

data WriteSubject = WriteSubject SubjectId ByteString UTCTime
  deriving (Eq, Show, Typeable, Generic, Binary)

-- Server definition

newtype SubjectLogServer = SubjectLogServer
  { subjectLogPid :: ProcessId }
  deriving (Eq, Show, Typeable, Generic, Binary)

instance Resolvable SubjectLogServer where
  resolve = return . Just . subjectLogPid

deriving instance Routable SubjectLogServer
deriving instance Linkable SubjectLogServer
deriving instance Addressable SubjectLogServer

-- ClientAPI

writeSubject :: SubjectLogServer -> SubjectId -> ByteString -> UTCTime -> Process (Maybe SubjectRecordId)
writeSubject log sid payload time = call log $ WriteSubject sid payload time

-- Handlers

handleWriteSubject :: SubjectLog -> WriteSubject -> Process (ProcessReply (Maybe SubjectRecordId) SubjectLog)
handleWriteSubject = undefined

-- Server

spawnSubjectLogServer :: Process SubjectLogServer
spawnSubjectLogServer = do
  pid <- spawnLocal $ serve () initLog subjectLogDef
  return $ SubjectLogServer pid
  where
    initLog :: InitHandler () SubjectLog
    initLog = \_ -> return $ InitOk SLog.empty Infinity

    subjectLogDef :: ProcessDefinition SubjectLog
    subjectLogDef = defaultProcess
      { apiHandlers = [
          handleCall handleWriteSubject
        ]
    , unhandledMessagePolicy = Drop }
