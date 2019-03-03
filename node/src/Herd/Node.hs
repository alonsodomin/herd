module Herd.Node
     ( startHerdNode
     , startHerdNode'
     ) where

import           Control.Applicative
import           Control.Concurrent     (threadDelay)
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Semigroup         ((<>))
import qualified Data.Text              as T
import           Transient.Base
import           Transient.Move
import           Transient.Move.Utils

import           Herd.Broker
import           Herd.Config
import           Herd.Core
import           Herd.HTTP

herdNode :: HerdConfig -> TransIO ()
herdNode config = do
  localNode <- mkNode $ config ^. hcCluster . ccBinding
  initWebApp localNode (httpApi localNode <|> broker <|> run localNode)

  where mkNode :: NetworkBinding -> TransIO Node
        mkNode binding = do
          let host = T.unpack $ binding ^. nbHost
          let port = binding ^. nbPort
          liftIO $ createNode host port

        httpApi :: Node -> Cloud ()
        httpApi self = local . async $ startHttpServer self (config ^. hcNetwork . ncHttp)

        broker :: Cloud ()
        broker = local . async $ startBroker (config ^. hcNetwork . ncBroker)

        run :: Node -> Cloud ()
        run node = do
          seedNodes <- local $ mapM mkNode $ config ^. hcCluster . ccSeedNodes
          forM_ seedNodes connect'
          runAt node . local $ herdApp

startHerdNode :: HerdConfig -> IO ()
startHerdNode config = void . keep $ do
  listNodes <|> herdNode config

  where listNodes :: TransIO ()
        listNodes = do
          _        <- option "nodes" "list currently connected nodes"
          allNodes <- getNodes
          forM_ allNodes printNode
          empty

        printNode :: Node -> TransIO ()
        printNode node = liftIO . putStrLn $ "- " <> (nodeHost node) <> ":" <> (show $ nodePort node)

startHerdNode' :: HerdConfig -> IO ()
startHerdNode' = void . keep' . herdNode
