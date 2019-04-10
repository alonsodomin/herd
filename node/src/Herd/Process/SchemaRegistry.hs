{-# LANGUAGE DeriveAnyClass #-}

module Herd.Process.SchemaRegistry
     ( SchemaRegistryServer
     , getSubjectIds
     , getVersions
     , getSchema
     , registerSchema
     , deleteSchema
     , spawnSchemaRegistry
     ) where

import           Control.Distributed.Process                (Process, ProcessId,
                                                             spawnLocal)
import           Control.Distributed.Process.Extras         hiding (sendChan)
import           Control.Distributed.Process.Extras.Time    (Delay (..))
import           Control.Distributed.Process.ManagedProcess
import           Control.Monad
import           Data.Avro.Schema                           (Schema)
import           Data.Binary                                (Binary (..))
import           Data.List.NonEmpty                         (NonEmpty)
import           Data.Typeable
import           GHC.Generics

import           Herd.Data.SchemaRegistry                   (SchemaRegistry)
import qualified Herd.Data.SchemaRegistry                   as Registry
import           Herd.Types

-- Protocol definition

data GetSubjectIds = GetSubjectIds
  deriving (Eq, Show, Generic, Typeable, Binary)

data GetVersions = GetVersions !SubjectId
  deriving (Eq, Show, Generic, Typeable, Binary)

data GetSchema = GetSchema !SubjectId !Version
  deriving (Eq, Show, Generic, Typeable, Binary)

data RegisterSchema = RegisterSchema !SubjectId !Schema
  deriving (Eq, Show, Generic, Typeable, Binary)

data DeleteSchema = DeleteSchema !SubjectId !Version
  deriving (Eq, Show, Generic, Typeable, Binary)

-- Server definition

newtype SchemaRegistryServer = SchemaRegistryServer
  { schemaRegistryPid :: ProcessId }
  deriving (Eq, Show, Generic, Typeable, Binary)

instance Resolvable SchemaRegistryServer where
  resolve = return . Just . schemaRegistryPid

deriving instance Routable SchemaRegistryServer
deriving instance Linkable SchemaRegistryServer
deriving instance Addressable SchemaRegistryServer

-- Client API

getSubjectIds :: SchemaRegistryServer -> Process [SubjectId]
getSubjectIds reg = call reg GetSubjectIds

getVersions :: SchemaRegistryServer -> SubjectId -> Process (Maybe (NonEmpty Version))
getVersions reg = call reg . GetVersions

getSchema :: SchemaRegistryServer -> SubjectId -> Version -> Process (Maybe Schema)
getSchema reg sid v = call reg $ GetSchema sid v

registerSchema :: SchemaRegistryServer -> SubjectId -> Schema -> Process ()
registerSchema reg sid sch = call reg $ RegisterSchema sid sch

deleteSchema :: SchemaRegistryServer -> SubjectId -> Version -> Process (Maybe ())
deleteSchema reg sid v = call reg $ DeleteSchema sid v

-- Handlers

handleGetSubjectIds :: SchemaRegistry -> GetSubjectIds -> Process (ProcessReply [SubjectId] SchemaRegistry)
handleGetSubjectIds reg _ = reply (Registry.getSubjects reg) reg

handleGetVersions :: SchemaRegistry -> GetVersions -> Process (ProcessReply (Maybe (NonEmpty Version)) SchemaRegistry)
handleGetVersions reg (GetVersions subjectId) =
  reply (Registry.getVersions subjectId reg) reg

handleGetSchema :: SchemaRegistry -> GetSchema -> Process (ProcessReply (Maybe Schema) SchemaRegistry)
handleGetSchema reg (GetSchema subjectId version) =
  reply (Registry.getSchema subjectId version reg) reg

handleRegisterSchema :: SchemaRegistry -> RegisterSchema -> Process (ProcessReply () SchemaRegistry)
handleRegisterSchema reg (RegisterSchema subjectId schema) =
  let newRegistry = Registry.registerSchema subjectId schema reg
  in reply () newRegistry

handleDeleteSchema :: SchemaRegistry -> DeleteSchema -> Process (ProcessReply (Maybe ()) SchemaRegistry)
handleDeleteSchema reg (DeleteSchema subjectId version) = do
  currentSchema <- pure $ Registry.getSchema subjectId version reg
  newRegistry   <- pure $ case currentSchema of
    Nothing -> reg
    Just _  -> Registry.deleteSchema subjectId version reg
  reply (void currentSchema) newRegistry

-- Server

spawnSchemaRegistry :: Process SchemaRegistryServer
spawnSchemaRegistry = do
  pid <- spawnLocal $ serve () initRegistry registryDef
  return $ SchemaRegistryServer pid
  where
    initRegistry :: InitHandler () SchemaRegistry
    initRegistry = \_ -> return $ InitOk Registry.empty Infinity

    registryDef :: ProcessDefinition SchemaRegistry
    registryDef = defaultProcess
      { apiHandlers = [
          handleCall handleGetSubjectIds
        , handleCall handleGetVersions
        , handleCall handleGetSchema
        , handleCall handleRegisterSchema
        , handleCall handleDeleteSchema
        ]
      , unhandledMessagePolicy = Drop }
