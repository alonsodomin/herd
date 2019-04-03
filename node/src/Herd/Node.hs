module Herd.Node
     ( startHerdNode
     ) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet (initializeBackend,
                                                                     newLocalNode)
import           Control.Distributed.Process.Node                   (LocalNode, initRemoteTable,
                                                                     runProcess)
import           Control.Lens
import qualified Data.Text                                          as T

import           Herd.Config
import           Herd.Process.SchemaRegistry

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
          return ()
