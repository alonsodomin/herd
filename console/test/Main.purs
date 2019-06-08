module Test.Main where

import Prelude

import Effect (Effect)
import Test.Data.Avro.Types (checkAvroType)

main :: Effect Unit
main = checkAvroType
