{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Protocol
     ( HerdRequest (..)
     , HerdResponse (..)
     , _GetSubjectIdsRes
     , _GetSchemaVersionsRes
     , _GetSchemaRes
     , _RegisterSchemaRes
     , _DeleteSchemaRes
     , HerdError (..)
     ) where

import           Control.Lens       hiding ((.=))
import           Data.Aeson
import qualified Data.Aeson.Types   as JSON
import           Data.Avro.Schema   (Schema)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import           Data.Text          (Text)
import           Data.Typeable
import qualified Data.Vector        as Vector
import           GHC.Generics
import           Network.JSONRPC

import           Herd.Types

data HerdRequest =
    GetSubjectIdsReq
  | GetSchemaVersionsReq SubjectId
  | GetSchemaReq SubjectId Version
  | RegisterSchemaReq SubjectId Schema
  | DeleteSchemaReq SubjectId Version
  deriving (Eq, Show, Typeable, Generic)

instance ToJSON HerdRequest where
  toJSON GetSubjectIdsReq = JSON.emptyArray
  toJSON (GetSchemaVersionsReq subjectId) = JSON.object [
      "subject-id" .= subjectId
    ]
  toJSON (GetSchemaReq subjectId version) = JSON.object [
      "subject-id" .= subjectId
    , "version"    .= version
    ]
  toJSON (RegisterSchemaReq subjectId schema) = JSON.object [
      "subject-id" .= subjectId
    , "schema"     .= schema
    ]
  toJSON (DeleteSchemaReq subjectId version) = JSON.object [
      "subject-id" .= subjectId
    , "version"    .= version
    ]

mnGetSubjectIds, mnGetSchemaVersions, mnGetSchema, mnRegisterSchema, mnDeleteSchema :: Text
mnGetSubjectIds     = "get-subject-ids"
mnGetSchemaVersions = "get-schema-versions"
mnGetSchema         = "get-schema"
mnRegisterSchema    = "register-schema"
mnDeleteSchema      = "delete-schema"

instance FromRequest HerdRequest where
  parseParams method
    | method == mnGetSubjectIds = Just . const $ return GetSubjectIdsReq

    | method == mnGetSchemaVersions = Just $
      withObject "get-schema-versions-req" $ \o -> do
        subjectId <- o .: "subject-id"
        return $ GetSchemaVersionsReq subjectId

    | method == mnGetSchema = Just $
      withObject "get-schema-req" $ \o -> do
        subjectId <- o .: "subject-id"
        version   <- o .: "version"
        return $ GetSchemaReq subjectId version

    | method == mnRegisterSchema = Just $
      withObject "register-schema-req" $ \o -> do
        subjectId <- o .: "subject-id"
        schema    <- o .: "schema"
        return $ RegisterSchemaReq subjectId schema

    | method == mnDeleteSchema = Just $
      withObject "delete-schema-req" $ \o -> do
        subjectId <- o .: "subject-id"
        version   <- o .: "version"
        return $ DeleteSchemaReq subjectId version

    | otherwise = Nothing

instance ToRequest HerdRequest where
  requestMethod GetSubjectIdsReq         = mnGetSubjectIds
  requestMethod (GetSchemaVersionsReq _) = mnGetSchemaVersions
  requestMethod (GetSchemaReq _ _)       = mnGetSchema
  requestMethod (RegisterSchemaReq _ _)  = mnRegisterSchema
  requestMethod (DeleteSchemaReq _ _)    = mnDeleteSchema

  requestIsNotif = const False

data HerdResponse =
    Done
  | GetSubjectIdsRes [SubjectId]
  | GetSchemaVersionsRes (NonEmpty Version)
  | GetSchemaRes Schema
  deriving (Eq, Show, Typeable, Generic)

makePrisms ''HerdResponse

_RegisterSchemaRes :: Prism' HerdResponse ()
_RegisterSchemaRes = _Done

_DeleteSchemaRes :: Prism' HerdResponse ()
_DeleteSchemaRes = _Done

instance ToJSON HerdResponse where
  toJSON Done                            = JSON.emptyArray
  toJSON (GetSubjectIdsRes subjectIds)   = JSON.Array . Vector.fromList $ fmap toJSON subjectIds
  toJSON (GetSchemaVersionsRes versions) = JSON.Array . Vector.fromList . NEL.toList $ fmap toJSON versions
  toJSON (GetSchemaRes schema)           = toJSON schema

instance FromResponse HerdResponse where
  parseResult method
    | method == mnGetSubjectIds = Just $
      withArray "subject-ids" $ \arr ->
        GetSubjectIdsRes <$> mapM parseJSON (Vector.toList arr)

    | method == mnGetSchemaVersions = Just $
      withArray "schema-versions" $ \arr ->
        GetSchemaVersionsRes . NEL.fromList <$> mapM parseJSON (Vector.toList arr)

    | method == mnGetSchema = Just $ \o -> GetSchemaRes <$> parseJSON o

    | method == mnRegisterSchema = Just . const $ return Done
    | method == mnDeleteSchema = Just . const $ return Done

    | otherwise = Nothing

data HerdError =
    SubjectNotFound SubjectId
  | SchemaNotFound SubjectId Version
  deriving (Eq, Show, Typeable, Generic, FromJSON, ToJSON)
