{-# LANGUAGE OverloadedStrings #-}

module Test.Herd.ClientSpec where

import           Test.Hspec

import           Herd.Client.API
import           Herd.Client.Stub
import           Herd.Protocol
import           Herd.Types

describeClient :: IO ()
describeClient = hspec $ do
  describe "getSubjectIds" $ do
    it "should return a list of subjectIds" $ do
      let subjectIds = ["foo", "bar"]
      let req = GetSubjectIdsReq

      receivedIds <- runClientStubT $ do
        -- configure the stub
        whenRequest $ req `replyWith` (GetSubjectIdsRes subjectIds)

        -- logic under test
        getSubjectIds

      receivedIds `shouldBe` subjectIds

