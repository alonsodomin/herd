{-# LANGUAGE DeriveAnyClass #-}

module Herd.Node
     ( startHerdNode
     ) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet (initializeBackend,
                                                                     newLocalNode)
import           Control.Distributed.Process.Node                   (LocalNode, initRemoteTable,
                                                                     runProcess)
import           Control.Lens
import           Control.Monad
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

startHerdNode :: HerdConfig -> IO ()
startHerdNode config = do
  node <- startNode
  runProcess node rootSupervisor

  where startNode :: IO LocalNode
        startNode = do
          let host = config ^. hcCluster . ccBinding . nbHost
          let port = show $ config ^. hcCluster . ccBinding . nbPort
          backend <- initializeBackend (T.unpack host) port initRemoteTable
          newLocalNode backend

        rootSupervisor :: Process ()
        rootSupervisor = do
          _ <- spawnLocal schemaRegistryProc
          forever $ receiveWait [match handleCmd]
