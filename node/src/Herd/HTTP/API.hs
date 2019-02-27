{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Herd.HTTP.API where

import           Data.Avro.Schema    (Schema)
import           Data.Time           (UTCTime)
import           Servant

import           Herd.Internal.Types

--type EventsAPI = "events" :> QueryParam "oldest" UTCTime :> Get '[JSON] [SubjectRecord]
type RegistryAPI = "schemas" :> Get '[JSON] [SubjectId]
              :<|> "schemas" :> Capture "subjectId" SubjectId :> Get '[JSON] [Version]
              :<|> "schemas" :> Capture "subjectId" SubjectId :> Capture "version" Version :> Get '[JSON] Schema

type HerdAPI = RegistryAPI

type RecordsAPI = "records" :> Get '[JSON] [SubjectRecord]

herdAPI :: Proxy HerdAPI
herdAPI = Proxy

instance FromHttpApiData SubjectId where
  parseUrlPiece = Right . SubjectId

instance FromHttpApiData Version where
  parseUrlPiece x = Version <$> parseUrlPiece x
