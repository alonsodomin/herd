{-# LANGUAGE TemplateHaskell #-}

module Herd.Core.Base
     ( HerdState
     , initialHerdState
     , hsRegistry
     , HerdBehaviour
     , dispatch
     ) where

import           Control.Lens
import           Control.Monad.State
import           Data.Typeable
import           Transient.Base
import           Transient.Move

import           Herd.Internal.Registry (RegistryState)
import qualified Herd.Internal.Registry as Registry

data HerdState = HerdState
  { _hsRegistry :: RegistryState
  } deriving (Eq, Show)

makeLenses ''HerdState

initialHerdState :: HerdState
initialHerdState = HerdState Registry.empty

type HerdBehaviour = StateT HerdState TransIO ()

dispatch :: (Typeable req, Typeable res, Show res, Read res) => Node -> req -> IO res
dispatch self req = do
  output <- keep' . oneThread . runCloud $ wormhole self handler
  case output of
    Just out -> return out
    Nothing  -> fail "impossible!"
  where handler = local $ do
          putMailbox req
          res <- getMailbox
          exit res
