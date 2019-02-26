module Herd.Node
     ( startHerdNode
     , startHerdNode'
     ) where

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent(threadDelay)
import Data.Semigroup ((<>))
import Transient.Base
import Transient.Move
import qualified Data.Text as T

import           Herd.Config
import Herd.HTTP

herdNode :: HerdConfig -> TransIO ()
herdNode config = runCloud $ do
  listen =<< (mkNode $ config ^. hcCluster . ccBinding)
  seedNodes <- mapM (mkNode >=> connect') $ config ^. hcCluster . ccSeedNodes
  return ()

  where mkNode :: NetworkBinding -> Cloud Node
        mkNode binding = do
          let host = T.unpack $ binding ^. nbHost
          let port = binding ^. nbPort
          localIO $ createNode host port

startHerdNode :: HerdConfig -> IO ()
startHerdNode config = void . keep $ do    
    herdNode config <|> listNodes

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
