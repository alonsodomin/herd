{-# LANGUAGE OverloadedStrings     #-}

module Test.Herd.Internal.Registry where

import           Control.Monad.State
import Data.List.NonEmpty (NonEmpty (..))
import Data.Avro.Schema (Schema)
import qualified Data.Avro.Schema as Avro
import Test.Hspec

import Herd.Internal.Registry
import Herd.Internal.Types

describeRegistry :: IO ()
describeRegistry = hspec $ do
  describe "empty registry" $ do
    it "should return an empty list of subjects" $ do
      subjects <- evalStateT getSubjects initial
      subjects `shouldBe` []
    
    it "should return no list of versions" $ do
      let subjectId = SubjectId "foo"
      versions <- evalStateT (getVersions subjectId) initial
      versions `shouldBe` Nothing

    it "should return no schema" $ do
      let subjectId = SubjectId "bar"
      let version   = initialVersion
      schema <- evalStateT (getSchema subjectId version) initial
      schema `shouldBe` Nothing

    it "should have a zero size" $ do
      totalItems <- evalStateT size initial
      totalItems `shouldBe` 0

    it "should register a new schema with the initial version" $ do
      let subjectId = SubjectId "foo"
      let schema    = Avro.Boolean

      let registerAndRetrieve = do
            registerSchema subjectId schema
            getSchema subjectId initialVersion
      
      retrieved <- evalStateT registerAndRetrieve initial
      retrieved `shouldBe` (Just schema)

    it "should bump schema version when registering a new one" $ do
      let subjectId = SubjectId "foo"
      let schema1   = Avro.Boolean
      let schema2   = Avro.mkUnion (Avro.Null :| [Avro.Boolean])

      let expectedVersion = Just $ nextVersion initialVersion

      let registerTwoSchemas = do
            registerSchema subjectId schema1
            registerSchema subjectId schema2
            finalSize     <- size
            latestVersion <- getLatestVersion subjectId
            return (finalSize, latestVersion)

      (sz, v) <- evalStateT registerTwoSchemas initial
      sz `shouldBe` 2
      v  `shouldBe` expectedVersion

    it "should delete an existent schema" $ do
      let subjectId = SubjectId "foo"
      let schema    = Avro.Boolean

      let registerAndDelete = do
            registerSchema subjectId schema
            deleteSchema subjectId initialVersion
            size

      finalSize <- evalStateT registerAndDelete initial
      finalSize `shouldBe` 0