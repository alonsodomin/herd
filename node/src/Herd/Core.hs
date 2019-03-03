module Herd.Core
     ( Dispatch
     , runDispatch
     , herdApp
     , module Herd.Core.Registry
     ) where

import           Control.Applicative
import           Control.Lens (zoom)
import           Control.Monad
import           Control.Monad.State
import           Data.Typeable
import           Transient.Base
import           Transient.Move

import           Herd.Core.Base
import           Herd.Core.Registry
import           Herd.Core.Storage

herdBehaviour :: HerdBehaviour
herdBehaviour = do
  mapStateT single $ zoom hsRegistry herdRegistry <|> zoom hsSubjectLog herdStorage
  --zoom hsRegistry herdRegistry <|> zoom hsSubjectLog herdStorage
  --single $ herdRegistry <|> herdStorage
  herdBehaviour

herdApp :: TransIO ()
herdApp = do
  --setState initialHerdState
  --herdBehaviour
  evalStateT herdBehaviour initialHerdState
