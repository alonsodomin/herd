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
initialHerdState = HerdState Registry.initial

type HerdBehaviour = StateT HerdState TransIO ()

dispatch :: (Typeable req, Typeable res, Show res, Read res) => Node -> req -> IO (Maybe res)
dispatch self req =
  keep' . oneThread . runCloud $ wormhole self handler
  where
    handler = local $ do
      putMailbox req
      res <- getMailbox
      exit res
