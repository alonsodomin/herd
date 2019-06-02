module Data.Avro.Schema.Types.Value where

import Data.Generic.Rep (class Generic)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.ByteString (ByteString)

data Value t =
    Null
  | Boolean Boolean
  | Int Int
  | Long Int
  | Float Number
  | Double Number
  | Bytes ByteString
  | String String

derive instance genericAvroValue :: Generic (Value t) _

instance encodeJsonAvroValue :: EncodeJson (Value t) where
  encodeJson Null = encodeJson "null"
  encodeJson (Boolean v) = encodeJson v
  encodeJson (Int v) = encodeJson v
  encodeJson (Long v) = encodeJson v
  encodeJson (Float v) = encodeJson v
  encodeJson (Double v) = encodeJson v
  encodeJson _ = jsonEmptyObject