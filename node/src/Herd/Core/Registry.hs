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
handleGetSubjects = behaviour $ \(GetSubjects) -> do
  state <- get
  lift $ evalStateT Registry.getSubjects state

data GetVersions = GetVersions SubjectId
  deriving (Eq, Show, Read, Typeable)

handleGetVersions :: RegistryBehaviour
handleGetVersions = behaviour $ \(GetVersions subjectId) -> do
  state <- get
  lift $ evalStateT (Registry.getVersions subjectId) state

data GetSchema = GetSchema SubjectId Version
  deriving (Eq, Show, Read, Typeable)

handleGetSchema :: RegistryBehaviour
handleGetSchema = behaviour $ \(GetSchema sid v) -> do
  state       <- get
  maybeSchema <- lift $ evalStateT (Registry.getSchema sid v) state
  return $ AvroSchema <$> maybeSchema

data GetLatestSchema = GetLatestSchema SubjectId
  deriving (Eq, Show, Read, Typeable)

handleGetLatestSchema :: RegistryBehaviour
handleGetLatestSchema = behaviour $ \(GetLatestSchema sid) -> do
  state       <- get
  maybeSchema <- lift $ evalStateT (findLatest sid) state
  return $ AvroSchema <$> maybeSchema
  where findLatest sid = do
          latestV <- Registry.getLatestVersion sid
          case latestV of
            Just v  -> Registry.getSchema sid v
            Nothing -> return Nothing

data DeleteSchema = DeleteSchema SubjectId Version
  deriving (Eq, Show, Read, Typeable)

handleDeleteSchema :: RegistryBehaviour
handleDeleteSchema = behaviour $ \(DeleteSchema sid v) -> do
  state                <- get
  (maybeSch, newState) <- lift $ runStateT (getAndDelete sid v) state
  put newState
  return $ AvroSchema <$> maybeSch
  where getAndDelete sid v = do
          schema <- Registry.getSchema sid v
          case schema of
            Just s  -> Registry.deleteSchema sid v >> return schema
            Nothing -> return Nothing

data RegisterSchema = RegisterSchema SubjectId AvroSchema
  deriving (Eq, Show, Read, Typeable)

handleRegisterSchema :: RegistryBehaviour
handleRegisterSchema = behaviour $ \(RegisterSchema sid sch) -> do
  state               <- get
  (latestV, newState) <- lift $ runStateT (registerAndGetVersion sid $ unwrapSchema sch) state
  put newState
  return latestV
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
