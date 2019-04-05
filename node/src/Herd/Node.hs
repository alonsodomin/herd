{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Herd.Node
     ( HerdNode
     , startHerdNode
     ) where

import           Control.Concurrent.STM                             (atomically)
import           Control.Concurrent.STM.TMVar
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet (initializeBackend,
                                                                     newLocalNode)
import           Control.Distributed.Process.Node                   (LocalNode, initRemoteTable,
                                                                     runProcess)
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binary                                        (Binary (..))
import qualified Data.Text                                          as T
import           Data.Typeable
import           GHC.Generics

import           Herd.Config
import           Herd.Process.SchemaRegistry

data NodeCommand = ShutdownCmd
  deriving (Eq, Show, Generic, Typeable, Binary)

handleCmd :: NodeCommand -> Process ()
handleCmd ShutdownCmd = say "Shutting down..."

data HerdNode = HerdNode
  { _hnLocalNode      :: LocalNode
  , _hnSchemaRegistry :: SchemaRegistryServer
  } deriving Typeable

makeLenses ''HerdNode

invoke :: MonadIO m => (HerdNode -> Process a) -> HerdNode -> m a
invoke action node = liftIO $ do
  tvar <- newEmptyTMVarIO
  runProcess (node ^. hnLocalNode) $ do
    result <- action node
    liftIO $ atomically $ putTMVar tvar result
  atomically $ readTMVar tvar

startHerdNode :: HerdConfig -> IO ()
startHerdNode config = do
  node <- startNode
  runProcess node rootSupervisor

  where startNode :: IO LocalNode
        startNode = do
          let host = T.unpack $ config ^. hcCluster . ccBinding . nbHost
          let port = show $ config ^. hcCluster . ccBinding . nbPort
          backend <- initializeBackend host port initRemoteTable
          newLocalNode backend

        rootSupervisor :: Process ()
        rootSupervisor = do
          _ <- spawnSchemaRegistry
          forever $ receiveWait [match handleCmd]
