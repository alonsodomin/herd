{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

module Herd.Core.Base
     ( HerdState
     , initialHerdState
     , hsRegistry
     , HerdBehaviour
     , Dispatch
     , dispatch
     , runDispatch
     ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Typeable
import           Transient.Base
import           Transient.Move         hiding (local)
import qualified Transient.Move         as Move

import           Herd.Internal.Registry (RegistryState)
import qualified Herd.Internal.Registry as Registry

data HerdState = HerdState
  { _hsRegistry :: RegistryState
  } deriving (Eq, Show)

makeLenses ''HerdState

initialHerdState :: HerdState
initialHerdState = HerdState Registry.empty

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
