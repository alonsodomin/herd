module Herd.Core.Registry
     ( AvroSchema (..)
     , herdRegistry
     , getSubjects
     , getVersions
     , getSchema
     , getLatestSchema
     , deleteSchema
     , registerSchema
     ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.Typeable
import           Transient.Base
import           Transient.Move

import           Herd.Core.Base
import           Herd.Internal.Registry (MemRegistry)
import qualified Herd.Internal.Registry as Registry
import           Herd.Internal.Types

type RegistryBehaviour = MemRegistry TransIO ()

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
  lift . putMailbox $ AvroSchema <$> maybeSchema

data GetLatestSchema = GetLatestSchema SubjectId
  deriving (Eq, Show, Read, Typeable)

handleGetLatestSchema :: RegistryBehaviour
handleGetLatestSchema = do
  (GetLatestSchema sid) <- lift (getMailbox :: TransIO GetLatestSchema)
  state                 <- get
  maybeSchema           <- lift $ evalStateT (findLatest sid) state
  lift . putMailbox $ AvroSchema <$> maybeSchema
  where findLatest sid = do
          latestV <- Registry.getLatestVersion sid
          case latestV of
            Just v  -> Registry.getSchema sid v
            Nothing -> return Nothing

data DeleteSchema = DeleteSchema SubjectId Version
  deriving (Eq, Show, Read, Typeable)

handleDeleteSchema :: RegistryBehaviour
handleDeleteSchema = do
  (DeleteSchema sid v) <- lift (getMailbox :: TransIO DeleteSchema)
  state                <- get
  (maybeSch, newState) <- lift $ runStateT (getAndDelete sid v) state
  put newState
  lift . putMailbox $ AvroSchema <$> maybeSch
  where getAndDelete sid v = do
          schema <- Registry.getSchema sid v
          case schema of
            Just s  -> Registry.deleteSchema sid v >> return schema
            Nothing -> return Nothing

data RegisterSchema = RegisterSchema SubjectId AvroSchema
  deriving (Eq, Show, Read, Typeable)

handleRegisterSchema :: RegistryBehaviour
handleRegisterSchema = do
  (RegisterSchema sid sch) <- lift $ (getMailbox :: TransIO RegisterSchema)
  state                    <- get
  (latestV, newState)      <- lift $ runStateT (registerAndGetVersion sid $ unwrapSchema sch) state
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
           <|> handleGetLatestSchema
           <|> handleDeleteSchema
           <|> handleRegisterSchema

-- Registry API

getSubjects :: Dispatch [SubjectId]
getSubjects = dispatch GetSubjects

getVersions :: SubjectId -> Dispatch (Maybe [Version])
getVersions subjectId = dispatch (GetVersions subjectId)

getSchema :: SubjectId -> Version -> Dispatch (Maybe AvroSchema)
getSchema sid v = dispatch (GetSchema sid v)

getLatestSchema :: SubjectId -> Dispatch (Maybe AvroSchema)
getLatestSchema sid = dispatch (GetLatestSchema sid)

deleteSchema :: SubjectId -> Version -> Dispatch (Maybe AvroSchema)
deleteSchema sid v = dispatch (DeleteSchema sid v)

registerSchema :: SubjectId -> AvroSchema -> Dispatch (Maybe Version)
registerSchema sid sch = dispatch (RegisterSchema sid sch)
