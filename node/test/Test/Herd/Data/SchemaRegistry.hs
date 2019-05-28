{-# LANGUAGE OverloadedStrings #-}

module Test.Herd.Data.SchemaRegistry where

import           Control.Monad.State
import           Data.Avro.Schema         (Schema)
import qualified Data.Avro.Schema         as Avro
import           Data.List.NonEmpty       (NonEmpty (..))
import           Test.Hspec

import           Herd.Data.SchemaRegistry
import           Herd.Types

describeRegistry :: IO ()
describeRegistry = hspec $ do
  describe "empty registry" $ do
    it "should return an empty list of subjects" $ do
      let subjects = getSubjects empty
      subjects `shouldBe` []

    it "should return no list of versions" $ do
      let subjectId = SubjectId "foo"
      let versions  = getVersions subjectId empty
      versions `shouldBe` Nothing

    it "should return no schema" $ do
      let subjectId = SubjectId "bar"
      let version   = initialVersion
      let schema    = getSchema subjectId version empty
      schema `shouldBe` Nothing

    it "should have a zero size" $ do
      let totalItems = size empty
      totalItems `shouldBe` 0

    it "should register a new schema with the initial version" $ do
      let subjectId = SubjectId "foo"
      let schema    = AvroSchema Avro.Boolean

      let registerAndRetrieve = do
            modify $ registerSchema subjectId schema
            getSchema subjectId initialVersion <$> get

      retrieved <- evalStateT registerAndRetrieve empty
      retrieved `shouldBe` (Just schema)

    it "should bump schema version when registering a new one" $ do
      let subjectId = SubjectId "foo"
      let schema1   = AvroSchema Avro.Boolean
      let schema2   = AvroSchema $ Avro.mkUnion (Avro.Null :| [Avro.Boolean])

      let expectedVersion = Just $ nextVersion initialVersion

      let registerTwoSchemas = do
            modify $ registerSchema subjectId schema1
            modify $ registerSchema subjectId schema2

            reg <- get
            let finalSize     = size reg
            let latestVersion = getLatestVersion subjectId reg

            return (finalSize, latestVersion)

      (sz, v) <- evalStateT registerTwoSchemas empty
      sz `shouldBe` 2
      v  `shouldBe` expectedVersion

    it "should delete an existent schema" $ do
      let subjectId = SubjectId "foo"
      let schema    = AvroSchema Avro.Boolean

      let registerAndDelete = do
            modify $ registerSchema subjectId schema
            modify $ deleteSchema subjectId initialVersion
            size <$> get

      finalSize <- evalStateT registerAndDelete empty
      finalSize `shouldBe` 0
