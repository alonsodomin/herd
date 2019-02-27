{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Herd.Core
     ( herdApp
     , getSubjects
     , getVersions
     ) where

import           Control.Lens
import Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Typeable
import           Transient.Base
import           Transient.Move

import           Herd.Internal.Registry (RegistryState)
import qualified Herd.Internal.Registry as Registry
import           Herd.Internal.Types

data HerdState = HerdState
  { _hsRegistry :: RegistryState
  } deriving (Eq, Show)

makeLenses ''HerdState

initialHerdState :: HerdState
initialHerdState = HerdState Registry.initial

type HerdApp = StateT HerdState TransIO

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

herdApp :: Cloud ()
herdApp = local $ evalStateT (handleGetSubjects <|> handleGetVersions) initialHerdState

dispatch :: (Typeable req, Typeable res, Show res, Read res) => Node -> req -> IO (Maybe res)
dispatch self req =
  keep' . oneThread . runCloud $  wormhole self handler
  where 
    handler = local $ do
      putMailbox req
      res <- getMailbox
      exit res

getSubjects :: Node -> IO [SubjectId]
getSubjects self = maybe [] id <$> dispatch self GetSubjects

getVersions :: Node -> SubjectId -> IO (Maybe [Version])
getVersions self subjectId = join <$> dispatch self (GetVersions subjectId)
