{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Herd.Internal.Storage.LocalStore
     ( ShardId (..)
     , Shard
     , createShard
     , LocalStoreT
     ) where

import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Hashable               (Hashable)
import           Data.Semigroup              ((<>))
import qualified Data.Text                   as T
import           Data.Text.Extra
import           Data.Typeable
import           GHC.Generics
import           System.Directory

import           Herd.Internal.Storage.Class
import           Herd.Internal.Types

newtype ShardId = ShardId Integer
  deriving (Eq, Show, Generic, Typeable)

instance Hashable ShardId

instance ToText ShardId where
  toText (ShardId num) = toText num

data Shard = Shard
  { _sShardId  :: ShardId
  , _sLocation :: FilePath
  , _sSeqNum   :: Integer
  } deriving (Eq, Show, Generic, Typeable)

makeLenses ''Shard

createShard :: ShardId -> FilePath -> IO Shard
createShard shardId basePath = do
  let location = basePath ++ "/" ++ (T.unpack $ toText shardId)
  createDirectoryIfMissing True location
  return $ Shard shardId location 0

newtype LocalStoreT m a = LocalStoreT
  { unLocalStore :: ReaderT Shard m a }
  deriving (Functor, Applicative, Monad)

instance (Monad m, MonadLogger m, MonadIO m) => MonadStorage (LocalStoreT m) where
  saveRecord = undefined
  loadRecords = undefined
