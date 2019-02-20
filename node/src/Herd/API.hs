{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Herd.API where

import           Data.Time           (UTCTime)
import           Servant

import           Herd.Internal.Types

--type EventsAPI = "events" :> QueryParam "oldest" UTCTime :> Get '[JSON] [EventRecord]
type EventsAPI = "events" :> Get '[JSON] [EventRecord]

eventsAPI :: Proxy EventsAPI
eventsAPI = Proxy

fetchEvents :: Maybe UTCTime -> Handler [EventRecord]
fetchEvents _ = return []
