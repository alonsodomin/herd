{-# LANGUAGE OverloadedStrings #-}

module Test.Herd.Types (describeTypes) where

import           Data.Aeson
import qualified Data.Aeson.Types as JSON
import           Data.Avro.Schema (Schema)
import qualified Data.Avro.Schema as Avro
import           Data.Text        (Text)
import           Test.Hspec

import           Herd.Types

describeTypes :: IO ()
describeTypes = describeAvroSchema

describeAvroSchema :: IO ()
describeAvroSchema = hspec $ do
  describe "AvroSchema" $ do
    it "should encode as a JSON string" $ do
      let schema    = AvroSchema Avro.Boolean
      toJSON schema `shouldBe` JSON.String "\"boolean\""
