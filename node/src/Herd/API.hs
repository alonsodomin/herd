{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Herd.API where

import           Data.Time           (UTCTime)
import           Servant

import           Herd.Internal.Types

--type EventsAPI = "events" :> QueryParam "oldest" UTCTime :> Get '[JSON] [SubjectRecord]
type RecordsAPI = "records" :> Get '[JSON] [SubjectRecord]

recordsAPI :: Proxy RecordsAPI
recordsAPI = Proxy

fetchRecords :: Maybe UTCTime -> Handler [SubjectRecord]
fetchRecords _ = return []
