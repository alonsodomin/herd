module Herd.HTTP.Storage where

import           Data.Time                (UTCTime)
import Servant

import Herd.Internal.Types

fetchRecords' :: Handler [SubjectRecord]
fetchRecords' = undefined
-- fetchEvents' systemRoot node = liftIO $ runProcess node $ do
--   self       <- getSelfPid
--   time       <- liftIO $ getCurrentTime
--   send systemRoot (self, loadRecordsMsg "foo-entity" time)
--   response   <- (expect :: Process StorageResponse)
--   return $ getRecords response



fetchRecords :: Maybe UTCTime -> Handler [SubjectRecord]
fetchRecords _ = return []