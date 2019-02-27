{-# LANGUAGE DeriveDataTypeable #-}

module Herd.Core.Registry
     ( herdRegistry
     , getSubjects
     , getVersions
     ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.Typeable
import           Transient.Base
import           Transient.Move

import           Herd.Core.Base
import           Herd.Internal.Registry (RegistryState)
import qualified Herd.Internal.Registry as Registry
import           Herd.Internal.Types

data GetSubjects = GetSubjects
  deriving (Eq, Show, Read, Typeable)

handleGetSubjects :: HerdApp ()
handleGetSubjects = do
  _        <- lift (getMailbox :: TransIO GetSubjects)
  state    <- get
  subjects <- lift $ evalStateT Registry.getSubjects (state ^. hsRegistry)
  lift $ putMailbox subjects

data GetVersions = GetVersions SubjectId
  deriving (Eq, Show, Read, Typeable)

handleGetVersions :: HerdApp ()
handleGetVersions = do
  (GetVersions subjectId) <- lift (getMailbox :: TransIO GetVersions)
  state                   <- get
  versions                <- lift $ evalStateT (Registry.getVersions subjectId) (state ^. hsRegistry)
  lift $ putMailbox versions

herdRegistry :: HerdApp ()
herdRegistry = handleGetSubjects <|> handleGetVersions

getSubjects :: Node -> IO [SubjectId]
getSubjects self = maybe [] id <$> dispatch self GetSubjects

getVersions :: Node -> SubjectId -> IO (Maybe [Version])
getVersions self subjectId = join <$> dispatch self (GetVersions subjectId)
