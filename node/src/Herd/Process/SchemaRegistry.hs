{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Herd.Process.SchemaRegistry
     ( getSubjects
     , schemaRegistryProc
     ) where

import           Control.Distributed.Process                (Process, ProcessId)
import           Control.Distributed.Process.Extras.Time    (Delay (..))
import           Control.Distributed.Process.ManagedProcess (InitResult (..), ProcessDefinition (..),
                                                             ProcessReply,
                                                             UnhandledMessagePolicy (..),
                                                             call,
                                                             defaultProcess,
                                                             handleCall, reply,
                                                             serve)
import           Data.Binary                                (Binary (..))
import           Data.Typeable
import           GHC.Generics

import           Herd.Data.SchemaRegistry                   (SchemaRegistry)
import qualified Herd.Data.SchemaRegistry                   as Registry
import           Herd.Internal.Types

-- Protocol definition

data GetSubjects = GetSubjects
  deriving (Eq, Show, Generic, Typeable, Binary)

-- Client API

getSubjects :: ProcessId -> Process [SubjectId]
getSubjects pid = call pid GetSubjects

-- Handlers

handleGetSubjects :: SchemaRegistry -> GetSubjects -> Process (ProcessReply [SubjectId] SchemaRegistry)
handleGetSubjects registry _ = reply (Registry.getSubjects registry) registry

-- Registry Process

schemaRegistryProc :: Process ()
schemaRegistryProc = serve () (\_ -> initRegistry) registryProc
  where
    initRegistry :: Process (InitResult SchemaRegistry)
    initRegistry = return $ InitOk Registry.empty NoDelay

    registryProc :: ProcessDefinition SchemaRegistry
    registryProc = defaultProcess
      { apiHandlers = [
          handleCall handleGetSubjects
        ]
      , unhandledMessagePolicy = Drop }
