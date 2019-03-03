{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

module Herd.Core.Base
     ( HerdState
     , initialHerdState
     , hsRegistry
     , hsSubjectLog
     , HerdBehaviour
     , Dispatch
     , dispatch
     , runDispatch
     , behaviour
     ) where

import           Control.Lens
import Control.Applicative
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Typeable
import           Transient.Base
import Transient.Logged (Loggable)
import           Transient.Move           hiding (local)
import qualified Transient.Move           as Move

import           Herd.Data.SchemaRegistry (SchemaRegistry)
import qualified Herd.Data.SchemaRegistry as Registry
import           Herd.Data.SubjectLog     (SubjectLog)
import qualified Herd.Data.SubjectLog     as SLog

data HerdState = HerdState
  { _hsRegistry   :: SchemaRegistry
  , _hsSubjectLog :: SubjectLog
  } deriving (Eq, Show)

makeLenses ''HerdState

initialHerdState :: HerdState
initialHerdState = HerdState Registry.empty SLog.empty

instance MonadBase TransIO TransIO where
  liftBase = id

type HerdBehaviour = StateT HerdState TransIO ()

newtype Dispatch a = Dispatch { unDispatch :: ReaderT Node IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Node)

dispatch :: (Typeable req, Typeable res, Loggable res) => req -> Dispatch res
dispatch req = do
  self   <- ask
  output <- liftIO $ keep' . oneThread . runCloud $ runAt self handler
  case output of
    Just out -> return out
    Nothing  -> fail "impossible!"
  where handler = Move.local $ do
          putMailbox req
          res <- getMailbox
          exit res

runDispatch :: Dispatch a -> Node -> IO a
runDispatch = runReaderT . unDispatch

behaviour :: (Typeable req, Typeable res, MonadBase TransIO m) => (req -> m res) -> m ()
behaviour f = do
  req <- liftBase getMailbox
  --liftBase $ cleanMailbox req
  res <- f req
  liftBase $ putMailbox res
