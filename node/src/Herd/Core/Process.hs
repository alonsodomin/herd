{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Herd.Core.Process
     ( ProcessId
     ) where

import           Data.Hashable
import           Data.Typeable
import           Data.UUID
import           GHC.Generics
import           System.Random

newtype ProcessId = ProcessId UUID
  deriving (Eq, Show, Ord, Hashable, Typeable, Generic)

newProcessId :: IO ProcessId
newProcessId = ProcessId <$> randomIO
