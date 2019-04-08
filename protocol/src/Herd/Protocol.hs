{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Herd.Protocol
     ( HerdRequest (..)
     , HerdResponse (..)
     , HerdError (..)
     ) where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Aeson.Types    as JSON
import           Data.Avro.Schema    (Schema)
import           Data.Typeable
import           GHC.Generics
import           Network.JSONRPC

import           Herd.Types

data HerdRequest =
    GetSubjectIds
  | GetSchemaVersions SubjectId
  | RegisterSchema SubjectId Schema
  deriving (Eq, Show, Typeable, Generic, FromJSON, ToJSON)

parseGetSubjectIdsReq :: JSON.Value -> JSON.Parser HerdRequest
parseGetSubjectIdsReq = const $ return GetSubjectIds

parseGetSchemaVersionsReq :: JSON.Value -> JSON.Parser HerdRequest
parseGetSchemaVersionsReq = withObject "get-schema-versions-req" $ \o -> do
  subjectId <- o .: "subject-id"
  return $ GetSchemaVersions subjectId

parseRegisterSchemaReq :: JSON.Value -> JSON.Parser HerdRequest
parseRegisterSchemaReq = withObject "register-schema-req" $ \o -> do
  subjectId <- o .: "subject-id"
  schema    <- o .: "schema"
  return $ RegisterSchema subjectId schema

instance FromRequest HerdRequest where
  parseParams "get-subject-ids"     = Just . const $ return GetSubjectIds
  parseParams "get-schema-versions" = Just parseJSON
  parseParams "register-schema"     = Just parseJSON
  parseParams _                     = Nothing

instance ToRequest HerdRequest where
  requestMethod GetSubjectIds         = "get-subject-ids"
  requestMethod (GetSchemaVersions _) = "get-schema-versions"
  requestMethod (RegisterSchema _ _)  = "register-schema"

  requestIsNotif = const False

data HerdResponse =
    Done
  | SubjectIds [SubjectId]
  | SchemaVersions [Version]
  deriving (Eq, Show, Typeable, Generic, FromJSON, ToJSON)

instance FromResponse HerdResponse where
  parseResult "get-subject-ids"     = Just parseJSON
  parseResult "get-schema-versions" = Just parseJSON
  parseResult "register-schema"     = Just . const $ return Done
  parseResult _                     = Nothing

data HerdError =
  SubjectNotFound SubjectId
  deriving (Eq, Show, Typeable, Generic, FromJSON, ToJSON)
