{-# LANGUAGE DeriveAnyClass    #-}

module Test.Herd.Internal.Types.Arbitrary where

import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Herd.Internal.Types

instance Arbitrary Version where
  arbitrary = Version <$> suchThat arbitrary (\x -> x >= 1)

deriving instance Arbitrary SubjectId
deriving instance Arbitrary SubjectRecordId
deriving instance Arbitrary SubjectRecord