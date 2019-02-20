{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Herd.Process.SchemaRegistry
     ( getSubjects
     , getVersions
     , getSchema
     -- Registry server
     , startSchemaRegistry
     ) where

import           Control.Distributed.Process                (Process, ProcessId)
import           Control.Distributed.Process.Extras.Time
import           Control.Distributed.Process.ManagedProcess
import qualified Data.Aeson                                 as JSON
import qualified Data.Aeson.Text                            as JSON
import           Data.Avro.Schema
import           Data.Binary                                (Binary (..))
import qualified Data.Binary                                as B
import           Data.ByteString                            (ByteString)
import qualified Data.ByteString.Lazy                       as BS
import           Data.Hashable                              (Hashable)
import           Data.HashMap.Lazy                          (HashMap)
import qualified Data.HashMap.Lazy                          as Map
import qualified Data.List.NonEmpty                         as NEL
import           Data.Map.NonEmpty                          (NEMap)
import qualified Data.Map.NonEmpty                          as NEM
import           Data.Text                                  (Text)
import           Data.Typeable
import           GHC.Generics

import           Herd.Internal.Types

type RegistryState = HashMap SubjectId (NEMap Version Schema)

instance Binary Schema where
  put = put . BS.toStrict . JSON.encode
  get = (get :: B.Get ByteString) >>= ((either fail pure) . JSON.eitherDecode' . BS.fromStrict)

-- Protocol Definition

data GetSubjects = GetSubjects
  deriving (Eq, Show, Generic, Typeable, Binary)

data GetVersions = GetVersions SubjectId
  deriving (Eq, Show, Generic, Typeable, Binary)

data GetSchema = GetSchema SubjectId Version
  deriving (Eq, Show, Generic, Typeable, Binary)

-- Client API

getSubjects :: ProcessId -> Process [SubjectId]
getSubjects pid = call pid GetSubjects

getVersions :: ProcessId -> SubjectId -> Process [Version]
getVersions pid = call pid . GetVersions

getSchema :: ProcessId -> SubjectId -> Version -> Process (Maybe Schema)
getSchema pid sid v = call pid $ GetSchema sid v

-- Handlers

handleGetSubjects :: RegistryState -> GetSubjects -> Process (ProcessReply [SubjectId] RegistryState)
handleGetSubjects registry GetSubjects = reply (Map.keys registry) registry

handleGetVersions :: RegistryState -> GetVersions -> Process (ProcessReply [Version] RegistryState)
handleGetVersions registry (GetVersions subjectId) = do
  versions <- pure $ maybe [] (NEL.toList . NEM.keys) $ Map.lookup subjectId registry
  reply versions registry

handleGetSchema :: RegistryState -> GetSchema -> Process (ProcessReply (Maybe Schema) RegistryState)
handleGetSchema registry (GetSchema subjectId version) = do
  versions <- pure $ Map.lookup subjectId registry
  reply (NEM.lookup version =<< versions) registry

-- Registry server

startSchemaRegistry :: Process ()
startSchemaRegistry = serve () (\() -> initRegistry) registryServer
  where
    initRegistry :: Process (InitResult RegistryState)
    initRegistry = return $ InitOk Map.empty NoDelay

    registryServer :: ProcessDefinition RegistryState
    registryServer = defaultProcess
      { apiHandlers = [
          handleCall handleGetSubjects
        , handleCall handleGetVersions
        , handleCall handleGetSchema
        ]
      , unhandledMessagePolicy = Drop
      }
