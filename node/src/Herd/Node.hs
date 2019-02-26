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

import           Herd.Config
import           Herd.HTTP

herdNode :: HerdConfig -> TransIO ()
herdNode config = do
  localNode <- mkNode $ config ^. hcCluster . ccBinding
  initWebApp localNode connectToSibilings

  where mkNode :: NetworkBinding -> TransIO Node
        mkNode binding = do
          let host = T.unpack $ binding ^. nbHost
          let port = binding ^. nbPort
          liftIO $ createNode host port

        connectToSibilings :: Cloud ()
        connectToSibilings = do
          seedNodes <- local $ mapM mkNode $ config ^. hcCluster . ccSeedNodes
          forM_ seedNodes connect'

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
