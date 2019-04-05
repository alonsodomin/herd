{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Herd.Protocol where

import           Data.Aeson
import           Data.Typeable
import           GHC.Generics
import           Network.JSONRPC

import           Herd.Types

data HerdRequest = FetchSubjectIds
  deriving (Eq, Show, Typeable, Generic, FromJSON, ToJSON)

instance FromRequest HerdRequest where
  parseParams "fetch-subject-ids" = Just . const $ return FetchSubjectIds
  parseParams _                   = Nothing

instance ToRequest HerdRequest where
  requestMethod FetchSubjectIds = "fetch-subject-ids"

  requestIsNotif = const False

data HerdResponse = FetchedSubjectIds [SubjectId]
  deriving (Eq, Show, Typeable, Generic, FromJSON, ToJSON)

instance FromResponse HerdResponse where
  parseResult "fetch-subject-ids" = Just parseJSON
  parseResult _                   = Nothing
