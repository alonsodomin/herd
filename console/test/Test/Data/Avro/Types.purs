module Test.Data.Avro.Types
  ( checkAvroType
  ) where

import Prelude

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Avro.Types (Type)
import Data.Either (Either(..))
import Effect (Effect)
import Test.QuickCheck (quickCheck)

checkAvroType :: Effect Unit
checkAvroType = quickCheck jsonRoundTrip

jsonRoundTrip :: Type -> Boolean
jsonRoundTrip typ = decodeJson (encodeJson typ) == Right typ