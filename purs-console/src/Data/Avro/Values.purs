module Data.Avro.Values where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmptyList)

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
