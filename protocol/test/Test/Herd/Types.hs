{-# LANGUAGE OverloadedStrings #-}

module Test.Herd.Types (describeTypes) where

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types    as JSON
import           Data.Avro.Schema    (Schema)
import qualified Data.Avro.Schema    as Avro
import qualified Data.HashMap.Strict as Map
import           Data.Text           (Text)
import           Test.Hspec

import           Herd.Types

describeTypes :: IO ()
describeTypes = describeAvroSchema

avroSchemaParams :: [(AvroSchema, JSON.Value)]
avroSchemaParams =
  [ (AvroSchema Avro.Null, JSON.String "null")
  , (AvroSchema Avro.Boolean, JSON.String "boolean")
  , (AvroSchema Avro.Int, JSON.String "int")
  , (AvroSchema Avro.Long, JSON.String "long")
  , (AvroSchema Avro.String, JSON.String "string")
  , (AvroSchema $ Avro.Array Avro.Int,
      JSON.Object $ Map.fromList [("type", JSON.String "array"), ("items", JSON.String "int")])
  ]

describeAvroSchema :: IO ()
describeAvroSchema = hspec $ do
  describe "AvroSchema" $ do
    forM_ avroSchemaParams describeAvroSchemaEncoding

  where
    describeAvroSchemaEncoding (schema, expected) =
      it ("should encode '" ++ (show schema) ++ "'") $ do
        toJSON schema `shouldBe` expected
