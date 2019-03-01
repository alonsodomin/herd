{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts            #-}

module Herd.Core.Base
     ( HerdState
     , initialHerdState
     , hsRegistry
     , hsStore
     , HerdBehaviour
     , Dispatch
     , dispatch
     , runDispatch
     , behaviour
     ) where

import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Typeable
import           Transient.Base
import           Transient.Move         hiding (local)
import qualified Transient.Move         as Move

import           Herd.Internal.Registry (RegistryState)
import qualified Herd.Internal.Registry as Registry
import           Herd.Internal.Storage  (StoreState)
import qualified Herd.Internal.Storage  as Store

data HerdState = HerdState
  { _hsRegistry :: RegistryState
  , _hsStore    :: StoreState
  } deriving (Eq, Show)

makeLenses ''HerdState

initialHerdState :: HerdState
initialHerdState = HerdState Registry.empty Store.empty

instance MonadBase TransIO TransIO where
  liftBase = id

type HerdBehaviour = StateT HerdState TransIO ()

newtype Dispatch a = Dispatch { unDispatch :: ReaderT Node IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Node)

dispatch :: (Typeable req, Typeable res, Show res, Read res) => req -> Dispatch res
dispatch req = do
  self   <- ask
  output <- liftIO $ keep' . oneThread . runCloud $ wormhole self handler
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
  req <- liftBase $ getMailbox
  res <- f req
  liftBase $ putMailbox res