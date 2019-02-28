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
  mainBehaviour
  herdBehaviour
  where mainBehaviour = do
         zoom hsRegistry herdRegistry <|> zoom hsStore herdStorage
         lift stop

herdApp :: Cloud ()
herdApp = local . (threads 10) $ do
  setState initialHerdState
  evalStateT herdBehaviour initialHerdState
