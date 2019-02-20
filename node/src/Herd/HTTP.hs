module Herd.HTTP where

import           Control.Lens
import qualified Network.Wai.Handler.Warp as Wai
import           Servant

import           Herd.API
import           Herd.Config
import           Herd.Internal.Types

httpServer :: Server EventsAPI
httpServer = fetchEvents'

fetchEvents' :: Handler [EventRecord]
fetchEvents' = undefined
-- fetchEvents' systemRoot node = liftIO $ runProcess node $ do
--   self       <- getSelfPid
--   time       <- liftIO $ getCurrentTime
--   send systemRoot (self, loadRecordsMsg "foo-entity" time)
--   response   <- (expect :: Process StorageResponse)
--   return $ getRecords response

startHttpServer :: NetworkBinding -> IO ()
startHttpServer net = do
  let httpPort = net ^. nbPort
  Wai.run httpPort $ serve eventsAPI httpServer
