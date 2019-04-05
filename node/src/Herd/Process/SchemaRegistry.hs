{-# LANGUAGE DeriveAnyClass #-}

module Herd.Process.SchemaRegistry
     ( SchemaRegistryServer
     , getSubjects
     , spawnSchemaRegistry
     ) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Extras         hiding (sendChan)
import           Control.Distributed.Process.Extras.Time    (Delay (..))
import           Control.Distributed.Process.ManagedProcess
import           Data.Binary                                (Binary (..))
import           Data.Typeable
import           GHC.Generics

import           Herd.Data.SchemaRegistry                   (SchemaRegistry)
import qualified Herd.Data.SchemaRegistry                   as Registry
import           Herd.Internal.Types

-- Protocol definition

data SchemaRegistryReq = GetSubjects !(SendPort [SubjectId])
  deriving (Eq, Show, Generic, Typeable, Binary)

-- Client API

getSubjects :: SchemaRegistryServer -> Process [SubjectId]
getSubjects reg = do
  (sp, rp) <- newChan
  let req = GetSubjects sp
  sendControlMessage (schemaRegistryInlet reg) req
  receiveWait [ matchChan rp return ]

-- Handlers

handleGetSubjects :: SchemaRegistry -> SchemaRegistryReq -> Process (ProcessAction SchemaRegistry)
handleGetSubjects registry (GetSubjects replyTo) = do
  let subjectIds = Registry.getSubjects registry
  replyChan replyTo subjectIds
  continue registry

-- Server definition

data SchemaRegistryServer = SchemaRegistryServer
  { schemaRegistryPid   :: ProcessId
  , schemaRegistryInlet :: ControlPort SchemaRegistryReq }
  deriving (Eq, Show, Generic, Typeable, Binary)

instance Resolvable SchemaRegistryServer where
  resolve = return . Just . schemaRegistryPid

deriving instance Routable SchemaRegistryServer
deriving instance Linkable SchemaRegistryServer
deriving instance Addressable SchemaRegistryServer

spawnSchemaRegistry :: Process SchemaRegistryServer
spawnSchemaRegistry = do
  (sp, rp)  <- newChan
  pid       <- spawnLocal $ runSchemaReg sp
  inletPort <- receiveChan rp
  return $ SchemaRegistryServer pid inletPort
  where
    initRegistry :: InitHandler () SchemaRegistry
    initRegistry = \_ -> return $ InitOk Registry.empty Infinity

    registryDef :: ControlChannel SchemaRegistryReq -> ProcessDefinition SchemaRegistry
    registryDef chan = defaultProcess
      { externHandlers = [
          handleControlChan chan handleGetSubjects
        ]
      , unhandledMessagePolicy = Drop }

    runSchemaReg :: SendPort (ControlPort SchemaRegistryReq) -> Process ()
    runSchemaReg sendPort = do
      inletChan <- newControlChan
      inletPort <- pure $ channelControlPort inletChan
      sendChan sendPort inletPort
      runProcess (recvLoop $ registryDef inletChan) () initRegistry
