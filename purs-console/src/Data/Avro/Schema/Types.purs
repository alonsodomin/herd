module Data.Avro.Schema.Types (Type) where

import Prelude

import Control.Alt ((<|>))
import Data.Generic (class Generic)
import Data.Either (Either(..))
import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Foldable (foldl)

data Type =
    Null
  | Boolean
  | Int
  | Long
  | Float
  | Double
  | Bytes
  | String

derive instance genericAvroType :: Generic Type

type Decoder a = Json -> Either String a

oneOfDecoder :: forall a. Array (Decoder a) -> Decoder a
oneOfDecoder decoders =
  \json -> foldl (\prev dec -> (dec json) <|> prev) (Left "Invalid type") decoders

instance decodeJsonAvroType :: DecodeJson Type where
  decodeJson = oneOfDecoder
    [ decodeNullType
    , decodeBooleanType
    , decodeIntType
    , decodeLongType
    , decodeFloatType
    , decodeDoubleType
    , decodeBytesType
    , decodeStringType
    ]

decodePrimitiveType :: String -> String -> Type -> Json -> Either String Type
decodePrimitiveType name errMsg typ json = do
  value <- decodeJson json
  if value == name then Right typ
    else Left errMsg

decodeNullType =
  decodePrimitiveType "null" "Not a null type" Null

decodeBooleanType =
  decodePrimitiveType "boolean" "Not a boolean type" Boolean

decodeIntType =
  decodePrimitiveType "int" "Not an int type" Int

decodeLongType =
  decodePrimitiveType "long" "Not a long type" Long

decodeFloatType =
  decodePrimitiveType "float" "Not a float type" Float

decodeDoubleType =
  decodePrimitiveType "double" "Not a double type" Double

decodeBytesType =
  decodePrimitiveType "bytes" "Not a bytes type" Bytes

decodeStringType =
  decodePrimitiveType "string" "Not an string type" String