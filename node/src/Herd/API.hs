{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Herd.API where

import           Data.Time           (UTCTime)
import           Servant

import           Herd.Internal.Types

--type EventsAPI = "events" :> QueryParam "oldest" UTCTime :> Get '[JSON] [SubjectRecord]
type HerdAPI = "subjects" :> Get '[JSON] [SubjectId]
          :<|> "subjects" :> Capture "subjectId" SubjectId :> Get '[JSON] [Version]
type RecordsAPI = "records" :> Get '[JSON] [SubjectRecord]

herdAPI :: Proxy HerdAPI
herdAPI = Proxy
