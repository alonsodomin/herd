{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Herd.Protocol
     ( HerdRequest (..)
     , HerdResponse (..)
     , HerdError (..)
     ) where

import           Data.Aeson
import qualified Data.Aeson.Types    as JSON
import           Data.Avro.Schema    (Schema)
import qualified Data.Vector as Vector
import           Data.Typeable
import           GHC.Generics
import           Network.JSONRPC

import           Herd.Types

data HerdRequest =
    GetSubjectIds
  | GetSchemaVersions SubjectId
  | RegisterSchema SubjectId Schema
  deriving (Eq, Show, Typeable, Generic)

instance ToJSON HerdRequest where
  toJSON GetSubjectIds = JSON.emptyArray
  toJSON (GetSchemaVersions subjectId) = JSON.object [
      "subject-id" .= subjectId
    ]
  toJSON (RegisterSchema subjectId schema) = JSON.object [
      "subject-id" .= subjectId
    , "schema"     .= schema
    ]

instance FromRequest HerdRequest where
  parseParams "get-subject-ids"     = Just . const $ return GetSubjectIds
  parseParams "get-schema-versions" = Just $
    withObject "get-schema-versions-req" $ \o -> do
      subjectId <- o .: "subject-id"
      return $ GetSchemaVersions subjectId
  parseParams "register-schema"     = Just $
    withObject "register-schema-req" $ \o -> do
      subjectId <- o .: "subject-id"
      schema    <- o .: "schema"
      return $ RegisterSchema subjectId schema
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
  deriving (Eq, Show, Typeable, Generic)

instance ToJSON HerdResponse where
  toJSON Done                      = JSON.emptyArray
  toJSON (SubjectIds subjectIds)   = JSON.Array . Vector.fromList $ fmap toJSON subjectIds
  toJSON (SchemaVersions versions) = JSON.Array . Vector.fromList $ fmap toJSON versions

instance FromResponse HerdResponse where
  parseResult "get-subject-ids"     = Just $
    withArray "subject-ids" $ \arr ->
      SubjectIds <$> mapM parseJSON (Vector.toList arr)
  parseResult "get-schema-versions" = Just $
    withArray "schema-versions" $ \arr ->
      SchemaVersions <$> mapM parseJSON (Vector.toList arr)
  parseResult "register-schema"     = Just . const $ return Done
  parseResult _                     = Nothing

data HerdError =
  SubjectNotFound SubjectId
  deriving (Eq, Show, Typeable, Generic, FromJSON, ToJSON)
