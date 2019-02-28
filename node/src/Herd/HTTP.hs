module Herd.HTTP
     ( startHttpServer
     ) where

import           Control.Lens
import qualified Network.Wai.Handler.Warp as Wai
import           Servant
import           Transient.Move

import           Herd.Config
import           Herd.Core
import           Herd.HTTP.API
import           Herd.HTTP.Registry

httpServer :: Node -> Server HerdAPI
httpServer = httpRegistry

startHttpServer :: Node -> NetworkBinding -> IO ()
startHttpServer node net = do
  let httpPort = net ^. nbPort
  Wai.run httpPort $ serve herdAPI (httpServer node)
