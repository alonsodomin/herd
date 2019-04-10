{-# LANGUAGE DeriveAnyClass #-}

module Herd.Process.SubjectLog
     ( SubjectLogServer
     , writeSubject
     , spawnSubjectLog
     ) where

import           Control.Distributed.Process                (Process, ProcessId,
                                                             spawnLocal)
import           Control.Distributed.Process.Extras         hiding (sendChan)
import           Control.Distributed.Process.Extras.Time    (Delay (..))
import           Control.Distributed.Process.ManagedProcess
import           Control.Lens
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

writeSubject :: SubjectId -> ByteString -> UTCTime -> SubjectLogServer ->  Process (Maybe SubjectRecordId)
writeSubject sid payload time slog = call slog $ WriteSubject sid payload time

-- Handlers

handleWriteSubject :: SubjectLog -> WriteSubject -> Process (ProcessReply (Maybe SubjectRecordId) SubjectLog)
handleWriteSubject slog (WriteSubject subjectId payload time) = do
  (record, newSLog) <- pure $ SLog.addRecord subjectId payload time slog
  reply (Just $ record ^. srSubjectRecordId) newSLog

-- Server

spawnSubjectLog :: Process SubjectLogServer
spawnSubjectLog = do
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
