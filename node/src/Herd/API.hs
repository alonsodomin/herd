{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Herd.API where

import           Data.Time           (UTCTime)
import           Servant

import           Herd.Internal.Types

--type EventsAPI = "events" :> QueryParam "oldest" UTCTime :> Get '[JSON] [SubjectRecord]
type HerdAPI = "subjects" :> Get '[JSON] [SubjectId]
type RecordsAPI = "records" :> Get '[JSON] [SubjectRecord]

herdAPI :: Proxy HerdAPI
herdAPI = Proxy

fetchRecords :: Maybe UTCTime -> Handler [SubjectRecord]
fetchRecords _ = return []

fetchSubjects :: Handler [SubjectId]
fetchSubjects = return []
