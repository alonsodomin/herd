{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Herd.Node
     ( HerdNode
     , startHerdNode
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

import           Herd.Node.Config
import           Herd.Node.Core
import           Herd.Node.RPC
import           Herd.Process.SchemaRegistry                        (spawnSchemaRegistry)
import           Herd.Process.SubjectLog                            (spawnSubjectLog)

startHerdNode :: HerdConfig -> IO ()
startHerdNode config = do
  node <- startNode
  runProcess node (rootSupervisor node)

  where startNode :: IO LocalNode
        startNode = do
          let host = T.unpack $ config ^. hcCluster . ccBinding . nbHost
          let port = show $ config ^. hcCluster . ccBinding . nbPort
          backend <- initializeBackend host port initRemoteTable
          newLocalNode backend

        rootSupervisor :: LocalNode -> Process ()
        rootSupervisor localNode = do
          reg  <- spawnSchemaRegistry
          slog <- spawnSubjectLog

          let herdNode = mkHerdNode localNode reg slog
          let herdEnv  = mkHerdEnv config herdNode

          liftIO $ startRpcServer config herdEnv
