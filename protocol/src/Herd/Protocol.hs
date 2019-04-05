{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Herd.Protocol where

import           Data.Aeson
import           Data.Avro.Schema (Schema)
import           Data.Typeable
import           GHC.Generics
import           Network.JSONRPC

import           Herd.Types

data HerdRequest =
    FetchSubjectIds
  | RegisterSchema SubjectId Schema
  deriving (Eq, Show, Typeable, Generic, FromJSON, ToJSON)

instance FromRequest HerdRequest where
  parseParams "fetch-subject-ids" = Just . const $ return FetchSubjectIds
  parseParams "register-schema"   = Just parseJSON
  parseParams _                   = Nothing

instance ToRequest HerdRequest where
  requestMethod FetchSubjectIds      = "fetch-subject-ids"
  requestMethod (RegisterSchema _ _) = "register-schema"

  requestIsNotif = const False

data HerdResponse =
    Done
  | FetchedSubjectIds [SubjectId]
  deriving (Eq, Show, Typeable, Generic, FromJSON, ToJSON)

instance FromResponse HerdResponse where
  parseResult "fetch-subject-ids" = Just parseJSON
  parseResult "register-schema"   = Just . const $ return Done
  parseResult _                   = Nothing
