module Data.Avro.Types.Value where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Data.Map (Map)
import Data.List.NonEmpty (NonEmptyList)
import Node.Encoding (Encoding(Base64))

data Value t =
    Null
  | Boolean Boolean
  | Int Int
  | Long Int
  | Float Number
  | Double Number
  | Bytes ByteString
  | String String
  | Array (Array (Value t))
  | Map (Map String (Value t))
  | Record t (Map String (Value t))
  | Union (NonEmptyList t) t (Value t)
  | Fixed t ByteString
  | Enum t String

derive instance genericAvroValue :: Generic (Value t) _

instance encodeJsonAvroValue :: EncodeJson (Value t) where
  encodeJson Null = encodeJson "null"
  encodeJson (Boolean v) = encodeJson v
  encodeJson (Int v) = encodeJson v
  encodeJson (Long v) = encodeJson v
  encodeJson (Float v) = encodeJson v
  encodeJson (Double v) = encodeJson v
  encodeJson (Bytes v) = encodeJson $ BS.toString v Base64
  encodeJson (String v) = encodeJson v
  encodeJson (Array v) = encodeJson $ map encodeJson v
  encodeJson (Map v) = encodeJson $ map encodeJson v
  encodeJson (Fixed _ v) = encodeJson $ BS.toString v Base64
  encodeJson (Enum _ v) = encodeJson v
  encodeJson (Union _ _ v) = encodeJson v
  encodeJson (Record _ v) = encodeJson $ map encodeJson v
