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
import           Herd.Data.SchemaRegistry (SchemaRegistry)
import qualified Herd.Data.SchemaRegistry as Registry
import           Herd.Internal.Types

--type RegistryBehaviour = TransIO ()
type RegistryBehaviour = StateT SchemaRegistry TransIO ()

-- Requests and handlers

data GetSubjects = GetSubjects
  deriving (Eq, Show, Read, Typeable)

handleGetSubjects :: RegistryBehaviour
handleGetSubjects = behaviour $ \(GetSubjects) -> do
  registry <- get
  return $ Registry.getSubjects registry

data GetVersions = GetVersions SubjectId
  deriving (Eq, Show, Read, Typeable)

handleGetVersions :: RegistryBehaviour
handleGetVersions = behaviour $ \(GetVersions subjectId) -> do
  registry <- get
  return $ Registry.getVersions subjectId registry

data GetSchema = GetSchema SubjectId Version
  deriving (Eq, Show, Read, Typeable)

handleGetSchema :: RegistryBehaviour
handleGetSchema = behaviour $ \(GetSchema sid v) -> do
  registry    <- get
  maybeSchema <- pure $ Registry.getSchema sid v registry
  return $ AvroSchema <$> maybeSchema

data GetLatestSchema = GetLatestSchema SubjectId
  deriving (Eq, Show, Read, Typeable)

handleGetLatestSchema :: RegistryBehaviour
handleGetLatestSchema = behaviour $ \(GetLatestSchema sid) -> do
  registry    <- get
  maybeSchema <- pure $ findLatest sid registry
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
  registry           <- get
  (maybeSch, newReg) <- pure $ getAndDelete sid v registry
  put newReg
  return $ AvroSchema <$> maybeSch
  where getAndDelete sid v reg =
          let foundSchema = Registry.getSchema sid v reg
          in case foundSchema of
              Just s  ->
                let newReg = Registry.deleteSchema sid v reg
                in (foundSchema, newReg)
              Nothing -> (Nothing, reg)

data RegisterSchema = RegisterSchema SubjectId AvroSchema
  deriving (Eq, Show, Read, Typeable)

handleRegisterSchema :: RegistryBehaviour
handleRegisterSchema = behaviour $ \(RegisterSchema sid sch) -> do
  registry          <- get
  (latestV, newReg) <- pure $ registerAndGetVersion sid (unwrapSchema sch) registry
  put newReg
  return latestV
  where registerAndGetVersion sid sch reg =
          let newReg  = Registry.registerSchema sid sch reg
              latestV = Registry.getLatestVersion sid newReg
          in (latestV, newReg)

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
