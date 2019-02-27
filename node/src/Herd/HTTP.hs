{-# LANGUAGE OverloadedStrings  #-}

module Herd.HTTP
     ( startHttpServer
     ) where

import           Control.Lens
import           Control.Monad.IO.Class   (liftIO)
import           Data.Time                (UTCTime)
import Data.Semigroup
import Data.Text.Extra
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as BS
import qualified Network.Wai.Handler.Warp as Wai
import           Servant
import           Transient.Move

import           Herd.API
import           Herd.Config
import           Herd.Core
import           Herd.Internal.Types

httpServer :: Node -> Server HerdAPI
httpServer node = (fetchSubjects node) :<|> (fetchVersions node)

instance FromHttpApiData SubjectId where
  parseUrlPiece = Right . SubjectId

fetchRecords' :: Handler [SubjectRecord]
fetchRecords' = undefined
-- fetchEvents' systemRoot node = liftIO $ runProcess node $ do
--   self       <- getSelfPid
--   time       <- liftIO $ getCurrentTime
--   send systemRoot (self, loadRecordsMsg "foo-entity" time)
--   response   <- (expect :: Process StorageResponse)
--   return $ getRecords response

fetchSubjects :: Node -> Handler [SubjectId]
fetchSubjects = liftIO . getSubjects

fetchVersions :: Node -> SubjectId -> Handler [Version]
fetchVersions node subjectId = do
  foundSubject <- liftIO $ getVersions node subjectId
  case foundSubject of
     Just vs -> return vs
     Nothing -> throwError $ err404 { errBody = BS.fromStrict . encodeUtf8 $ "Subject '" <> (toText subjectId) <> "' not found." }

fetchRecords :: Maybe UTCTime -> Handler [SubjectRecord]
fetchRecords _ = return []

startHttpServer :: Node -> NetworkBinding -> IO ()
startHttpServer node net = do
  let httpPort = net ^. nbPort
  Wai.run httpPort $ serve herdAPI (httpServer node)
