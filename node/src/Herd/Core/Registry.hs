{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Herd.Core.Registry
     ( herdRegistry
     , getSubjects
     , getVersions
     , getSchema
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

-- Registry module definition

herdRegistry :: RegistryBehaviour
herdRegistry = handleGetSubjects <|> handleGetVersions <|> handleGetSchema

-- Client API

getSubjects :: Node -> IO [SubjectId]
getSubjects self = dispatch self GetSubjects

getVersions :: Node -> SubjectId -> IO (Maybe [Version])
getVersions self subjectId = dispatch self (GetVersions subjectId)

getSchema :: Node -> SubjectId -> Version -> IO (Maybe Schema)
getSchema self sid v = dispatch self (GetSchema sid v)
