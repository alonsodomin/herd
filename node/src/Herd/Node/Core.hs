{-# LANGUAGE TemplateHaskell #-}

module Herd.Node.Core where

import           Control.Concurrent.STM           (atomically)
import           Control.Concurrent.STM.TMVar
import           Control.Distributed.Process      (Process)
import           Control.Distributed.Process.Node (LocalNode, runProcess)
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Typeable

import           Herd.Process.SchemaRegistry      (SchemaRegistryServer)

data HerdNode = HerdNode
  { _hnLocalNode      :: LocalNode
  , _hnSchemaRegistry :: SchemaRegistryServer
  } deriving Typeable

makeLenses ''HerdNode

invokeHerd :: MonadIO m => (HerdNode -> Process a) -> HerdNode -> m a
invokeHerd action node = liftIO $ do
  tvar <- newEmptyTMVarIO
  runProcess (node ^. hnLocalNode) $ do
    result <- action node
    liftIO $ atomically $ putTMVar tvar result
  atomically $ readTMVar tvar
