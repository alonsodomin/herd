module Herd.Node
     ( startHerdNode
     ) where

import Control.Lens

import           Herd.Config
import Herd.HTTP

startHerdNode :: HerdConfig -> IO ()
--startHerdNode config = startHttpServer $ config ^. hcNetwork . ncHttp
startHerdNode config = undefined
