{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Herd.Node.Core
     ( HerdNode
     , mkHerdNode
     , hnLocalNode
     , hnSchemaRegistry
     , NodeActionT
     , runNodeActionT
     , invokeAction
     ) where

import           Control.Concurrent.STM           (atomically)
import           Control.Concurrent.STM.TMVar
import           Control.Distributed.Process      (Process)
import           Control.Distributed.Process.Node (LocalNode, runProcess)
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Typeable

import           Herd.Process.SchemaRegistry      (SchemaRegistryServer)

data HerdNode = HerdNode
  { _hnLocalNode      :: LocalNode
  , _hnSchemaRegistry :: SchemaRegistryServer
  } deriving Typeable

makeLenses ''HerdNode

mkHerdNode :: LocalNode -> SchemaRegistryServer -> HerdNode
mkHerdNode = HerdNode

newtype NodeActionT m a = NodeActionT
  { unNodeActionT :: ReaderT HerdNode m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HerdNode, MonadTrans, MonadLogger)

runNodeActionT :: HerdNode -> NodeActionT m a -> m a
runNodeActionT node act = runReaderT (unNodeActionT act) node

invokeAction :: MonadIO m => (HerdNode -> Process a) -> NodeActionT m a
invokeAction action = NodeActionT $ do
  node <- ask
  liftIO $ do
    tvar <- newEmptyTMVarIO
    runProcess (node ^. hnLocalNode) $ do
      result <- action node
      liftIO $ atomically $ putTMVar tvar result
    atomically $ readTMVar tvar
