{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Herd.Core
     ( herdApp
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

herdApp :: Cloud ()
herdApp = local $ evalStateT herdRegistry initialHerdState

