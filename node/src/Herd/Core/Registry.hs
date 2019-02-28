{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Herd.Core.Registry
     ( herdRegistry
     , getSubjects
     , getVersions
     , getSchema
     , deleteSchema
     , registerSchema
     ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.Avro.Schema       (Schema)
import           Data.Typeable
import           Transient.Base
import           Transient.Move

import           Herd.Core.Base
import           Herd.Internal.Registry (MemRegistry)
import qualified Herd.Internal.Registry as Registry
import           Herd.Internal.Types

type RegistryBehaviour = MemRegistry TransIO ()

instance Read Schema

-- Requests and handlers

data GetSubjects = GetSubjects
  deriving (Eq, Show, Read, Typeable)

handleGetSubjects :: RegistryBehaviour
handleGetSubjects = do
  _        <- lift (getMailbox :: TransIO GetSubjects)
  state    <- get
  subjects <- lift $ evalStateT Registry.getSubjects state
  lift $ putMailbox subjects

data GetVersions = GetVersions SubjectId
  deriving (Eq, Show, Read, Typeable)

handleGetVersions :: RegistryBehaviour
handleGetVersions = do
  (GetVersions subjectId) <- lift (getMailbox :: TransIO GetVersions)
  state                   <- get
  versions                <- lift $ evalStateT (Registry.getVersions subjectId) state
  lift $ putMailbox versions

data GetSchema = GetSchema SubjectId Version
  deriving (Eq, Show, Read, Typeable)

handleGetSchema :: RegistryBehaviour
handleGetSchema = do
  (GetSchema sid v) <- lift (getMailbox :: TransIO GetSchema)
  state             <- get
  maybeSchema       <- lift $ evalStateT (Registry.getSchema sid v) state
  lift $ putMailbox maybeSchema

data DeleteSchema = DeleteSchema SubjectId Version
  deriving (Eq, Show, Read, Typeable)

handleDeleteSchema :: RegistryBehaviour
handleDeleteSchema = do
  (DeleteSchema sid v) <- lift (getMailbox :: TransIO DeleteSchema)
  state                <- get
  (maybeSch, newState) <- lift $ runStateT (getAndDelete sid v) state
  put newState
  lift $ putMailbox maybeSch
  where getAndDelete sid v = do
          schema <- Registry.getSchema sid v
          case schema of
            Just s  -> Registry.deleteSchema sid v >> return schema
            Nothing -> return Nothing

data RegisterSchema = RegisterSchema SubjectId Schema
  deriving (Eq, Show, Read, Typeable)

handleRegisterSchema :: RegistryBehaviour
handleRegisterSchema = do
  (RegisterSchema sid sch) <- lift $ (getMailbox :: TransIO RegisterSchema)
  state                    <- get
  (latestV, newState)      <- lift $ runStateT (registerAndGetVersion sid sch) state
  put newState
  lift $ putMailbox latestV
  where registerAndGetVersion sid sch = do
          Registry.registerSchema sid sch
          Registry.getLatestVersion sid

-- Registry module definition

herdRegistry :: RegistryBehaviour
herdRegistry = handleGetSubjects
           <|> handleGetVersions
           <|> handleGetSchema
           <|> handleDeleteSchema
           <|> handleRegisterSchema

-- Registry API

getSubjects :: Dispatch [SubjectId]
getSubjects = dispatch GetSubjects

getVersions :: SubjectId -> Dispatch (Maybe [Version])
getVersions subjectId = dispatch (GetVersions subjectId)

getSchema :: SubjectId -> Version -> Dispatch (Maybe Schema)
getSchema sid v = dispatch (GetSchema sid v)

deleteSchema :: SubjectId -> Version -> Dispatch (Maybe Schema)
deleteSchema sid v = dispatch (DeleteSchema sid v)

registerSchema :: SubjectId -> Schema -> Dispatch (Maybe Version)
registerSchema sid sch = dispatch (RegisterSchema sid sch)
