module Herd.Core
     ( Dispatch
     , runDispatch
     , herdApp
     , module Herd.Core.Registry
     ) where

import           Control.Applicative
import           Control.Lens
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
  --zoom hsRegistry herdRegistry <|> zoom hsStore herdStorage
  herdRegistry <|> herdStorage
  herdBehaviour

herdApp :: Cloud ()
herdApp = local $ do
  setState initialHerdState
  herdBehaviour
  --evalStateT herdBehaviour initialHerdState
