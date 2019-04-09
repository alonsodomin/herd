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
     , _ReadSubjectRes
     , _WriteSubjectRes
     , HerdError (..)
     ) where

import           Control.Lens                hiding ((.=))
import           Data.Aeson
import qualified Data.Aeson.Types            as JSON
import           Data.Avro.Schema            (Schema)
import           Data.Binary.Orphans         ()
import qualified Data.ByteString.Base64.Lazy as Base64
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as BSL
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as NEL
import           Data.Text                   (Text)
import           Data.Time.Clock             (UTCTime)
import           Data.Typeable
import qualified Data.Vector                 as Vector
import           GHC.Generics
import           Network.JSONRPC

import           Herd.Types

data HerdRequest =
    GetSubjectIdsReq
  | GetSchemaVersionsReq SubjectId
  | GetSchemaReq SubjectId Version
  | RegisterSchemaReq SubjectId Schema
  | DeleteSchemaReq SubjectId Version
  --
  | ReadSubjectReq SubjectId UTCTime
  | WriteSubjectReq SubjectId ByteString
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
  toJSON (ReadSubjectReq subjectId from) = JSON.object [
      "subject-id" .= subjectId
    , "from"       .= from
    ]
  toJSON (WriteSubjectReq subjectId payload) = JSON.object [
      "subject-id" .= subjectId
    , "payload"    .= (BSL.unpack . Base64.encode $ payload)
    ]

mnGetSubjectIds, mnGetSchemaVersions, mnGetSchema, mnRegisterSchema, mnDeleteSchema :: Text
mnReadSubject, mnWriteSubject :: Text
mnGetSubjectIds     = "get-subject-ids"
mnGetSchemaVersions = "get-schema-versions"
mnGetSchema         = "get-schema"
mnRegisterSchema    = "register-schema"
mnDeleteSchema      = "delete-schema"
mnReadSubject       = "read-subject"
mnWriteSubject      = "write-subject"

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
        ver       <- o .: "version"
        return $ GetSchemaReq subjectId ver

    | method == mnRegisterSchema = Just $
      withObject "register-schema-req" $ \o -> do
        subjectId <- o .: "subject-id"
        schema    <- o .: "schema"
        return $ RegisterSchemaReq subjectId schema

    | method == mnDeleteSchema = Just $
      withObject "delete-schema-req" $ \o -> do
        subjectId <- o .: "subject-id"
        ver       <- o .: "version"
        return $ DeleteSchemaReq subjectId ver

    | method == mnReadSubject = Just $
      withObject "read-subject-req" $ \o -> do
        subjectId <- o .: "subject-id"
        from      <- o .: "from"
        return $ ReadSubjectReq subjectId from

    | method == mnWriteSubject = Just $
      withObject "write-subject-req" $ \o -> do
        subjectId <- o .: "subject-id"
        payload   <- (Base64.encode . BSL.pack) <$> o .: "payload"
        return $ WriteSubjectReq subjectId payload

    | otherwise = Nothing

instance ToRequest HerdRequest where
  requestMethod GetSubjectIdsReq         = mnGetSubjectIds
  requestMethod (GetSchemaVersionsReq _) = mnGetSchemaVersions
  requestMethod (GetSchemaReq _ _)       = mnGetSchema
  requestMethod (RegisterSchemaReq _ _)  = mnRegisterSchema
  requestMethod (DeleteSchemaReq _ _)    = mnDeleteSchema

  requestMethod (ReadSubjectReq _ _)     = mnReadSubject
  requestMethod (WriteSubjectReq _ _)    = mnWriteSubject

  requestIsNotif = const False

data HerdResponse =
    Done
  | GetSubjectIdsRes [SubjectId]
  | GetSchemaVersionsRes (NonEmpty Version)
  | GetSchemaRes Schema
  --
  | ReadSubjectRes [SubjectRecord]
  | WriteSubjectRes SubjectRecordId
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
  toJSON (ReadSubjectRes records)        = toJSON records
  toJSON (WriteSubjectRes subjectId)     = toJSON subjectId

instance FromResponse HerdResponse where
  parseResult method
    | method == mnGetSubjectIds = Just $
      withArray "subject-ids" $ \arr ->
        GetSubjectIdsRes <$> mapM parseJSON (Vector.toList arr)

    | method == mnGetSchemaVersions = Just $
      withArray "schema-versions" $ \arr ->
        GetSchemaVersionsRes . NEL.fromList <$> mapM parseJSON (Vector.toList arr)

    | method == mnGetSchema = Just $ \x -> GetSchemaRes <$> parseJSON x

    | method == mnRegisterSchema = Just . const $ return Done
    | method == mnDeleteSchema = Just . const $ return Done

    | method == mnReadSubject = Just $
      withArray "subject-records" $ \arr ->
        ReadSubjectRes <$> mapM parseJSON (Vector.toList arr)
    | method == mnWriteSubject = Just $ \x -> WriteSubjectRes <$> parseJSON x

    | otherwise = Nothing

data HerdError =
    SubjectNotFound SubjectId
  | SchemaNotFound SubjectId Version
  deriving (Eq, Show, Typeable, Generic, FromJSON, ToJSON)
