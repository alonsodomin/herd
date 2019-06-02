module Data.Argonaut.Generic where

import Prelude

import Data.Argonaut.Core (Json, caseJson)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Class (class Encode, encode)
import Foreign.Object (Object)
import Foreign.Object as Object

jsonToForeign :: Json -> Foreign
jsonToForeign = caseJson
  (const $ unsafeToForeign {})
  (\b -> unsafeToForeign b)
  (\x -> unsafeToForeign x)
  (\s -> unsafeToForeign s)
  (\xs -> unsafeToForeign $ map jsonToForeign xs)
  (\obj -> unsafeToForeign $ Object.mapWithKey (\_ -> jsonToForeign) obj)

