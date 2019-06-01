module Data.Avro.Schema.Types
  ( Type
  , TypeName
  , Order
  , Field
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Generic (class Generic)
import Data.Either (Either(..))
import Data.Argonaut.Core (Json, JObject, toObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, getField)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL

newtype TypeName =
  TypeName String

derive instance genericAvroTypeName :: Generic TypeName

instance decodeJsonAvroTypeName :: DecodeJson TypeName where
  decodeJson json = TypeName <$> decodeJson json

instance encodeJsonAvroTypeName :: EncodeJson TypeName where
  encodeJson (TypeName value) = encodeJson value

data Type =
    Null
  | Boolean
  | Int
  | Long
  | Float
  | Double
  | Bytes
  | String
  | Array { items :: Type }
  | Map { values :: Type }
  | Union { options :: NonEmptyList Type }
  | Fixed {
      name :: TypeName
    , namespace :: Maybe String
    , aliases :: List TypeName
    , size :: Int
    }
  | Enum {
      name :: TypeName
    , namespace :: Maybe String
    , aliases :: List TypeName
    , doc :: Maybe String
    , symbols :: NonEmptyList TypeName
    }
  | Record {
      name :: TypeName
    , namespace :: Maybe String
    , aliases :: List TypeName
    , doc :: Maybe String
    , order :: Maybe Order
    , fields :: List Field
    }

derive instance genericAvroType :: Generic Type

type Decoder a = Json -> Either String a

type TypeDecoder = Decoder Type

oneOfDecoder :: forall a. Array (Decoder a) -> Decoder a
oneOfDecoder decoders =
  \json -> foldl (\prev dec -> (dec json) <|> prev) (Left "Invalid type") decoders

jsonObject :: Json -> Either String JObject
jsonObject json = maybe (Left "Not a JSON object") Right (toObject json)

jsonNonEmptyList :: forall a. DecodeJson a => Decoder (NonEmptyList a)
jsonNonEmptyList json = do
  list <- decodeJson json
  case (NEL.fromList list) of
    Just ls -> Right ls
    Nothing -> Left "List can not be empty"

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
    , decodeArrayType
    , decodeMapType
    , decodeUnionType
    , decodeFixedType
    , decodeEnumType
    , decodeRecordType
    ]

instance encodeJsonAvroType :: EncodeJson Type where
  encodeJson Null = encodeJson "null"
  encodeJson Boolean = encodeJson "boolean"
  encodeJson Int = encodeJson "int"
  encodeJson Long = encodeJson "long"
  encodeJson Float = encodeJson "float"
  encodeJson Double = encodeJson "double"
  encodeJson Bytes = encodeJson "bytes"
  encodeJson String = encodeJson "string"
  encodeJson (Array { items }) =
    "items" := items
    ~> jsonEmptyObject
  encodeJson (Map { values }) =
    "values" := values
    ~> jsonEmptyObject
  encodeJson (Union { options }) =
    encodeJson (NEL.toList options)
  encodeJson (Fixed fixed) =
    "name" := fixed.name
    ~> "namespace" := fixed.namespace
    ~> "aliases" := fixed.aliases
    ~> "size" := fixed.size
    ~> jsonEmptyObject
  encodeJson (Enum enum) =
    "name" := enum.name
    ~> "namespace" := enum.namespace
    ~> "aliases" := enum.aliases
    ~> "doc" := enum.doc
    ~> "symbols" := encodeJson (NEL.toList enum.symbols)
    ~> jsonEmptyObject
  encodeJson (Record rec) =
    "name" := rec.name
    ~> "namespace" := rec.namespace
    ~> "aliases" := rec.aliases
    ~> "doc" := rec.doc
    ~> "order" := rec.order
    ~> "fields" := rec.fields
    ~> jsonEmptyObject

decodePrimitiveType :: String -> String -> Type -> TypeDecoder
decodePrimitiveType name errMsg typ json = do
  value <- decodeJson json
  if value == name then Right typ
    else Left errMsg

decodeNullType :: TypeDecoder
decodeNullType =
  decodePrimitiveType "null" "Not a null type" Null

decodeBooleanType :: TypeDecoder
decodeBooleanType =
  decodePrimitiveType "boolean" "Not a boolean type" Boolean

decodeIntType :: TypeDecoder
decodeIntType =
  decodePrimitiveType "int" "Not an int type" Int

decodeLongType :: TypeDecoder
decodeLongType =
  decodePrimitiveType "long" "Not a long type" Long

decodeFloatType :: TypeDecoder
decodeFloatType =
  decodePrimitiveType "float" "Not a float type" Float

decodeDoubleType :: TypeDecoder
decodeDoubleType =
  decodePrimitiveType "double" "Not a double type" Double

decodeBytesType :: TypeDecoder
decodeBytesType =
  decodePrimitiveType "bytes" "Not a bytes type" Bytes

decodeStringType :: TypeDecoder
decodeStringType =
  decodePrimitiveType "string" "Not an string type" String

decodeArrayType :: TypeDecoder
decodeArrayType json = do
  obj <- jsonObject json
  typ <- getField obj "items"
  pure (Array { items: typ })

decodeMapType :: TypeDecoder
decodeMapType json = do
  obj <- jsonObject json
  typ <- getField obj "values"
  pure (Map { values: typ })

decodeUnionType :: TypeDecoder
decodeUnionType json = do
  values <- jsonNonEmptyList json
  pure $ Union { options: values }

decodeFixedType :: TypeDecoder
decodeFixedType json = do
  obj <- jsonObject json
  name <- getField obj "name"
  namespace <- getField obj "namespace"
  aliases <- getField obj "aliases"
  size <- getField obj "size"
  pure $ Fixed { name: name, namespace: namespace, aliases: aliases, size: size }

decodeEnumType :: TypeDecoder
decodeEnumType json = do
  obj <- jsonObject json
  name <- getField obj "name"
  namespace <- getField obj "namespace"
  aliases <- getField obj "aliases"
  doc <- getField obj "doc"
  maybeSymbols <- NEL.fromList <$> getField obj "symbols"
  case maybeSymbols of
    Just symbols ->
      pure $ Enum { name: name, namespace: namespace, aliases: aliases, doc: doc, symbols: symbols }
    Nothing ->
      Left "Symbol list for Enum can not be empty"

decodeRecordType :: TypeDecoder
decodeRecordType json = do
  obj <- jsonObject json
  name <- getField obj "name"
  namespace <- getField obj "namespace"
  aliases <- getField obj "aliases"
  doc <- getField obj "doc"
  order <- getField obj "order"
  fields <- getField obj "fields"
  pure $ Record {
    name: name
  , aliases: aliases
  , namespace: namespace
  , doc: doc
  , order: order
  , fields: fields
  }

-- Order

data Order =
    Ascending
  | Descending
  | Ignore

derive instance genericAvroOrder :: Generic Order

instance decodeJsonAvroOrder :: DecodeJson Order where
  decodeJson json = do
    value <- decodeJson json
    case value of
      "ascending" -> Right Ascending
      "descending" -> Right Descending
      "ignore" -> Right Ignore
      _ -> Left $ "Not a valid order value: " <> value

instance encodeJsonAvroOrder :: EncodeJson Order where
  encodeJson Ascending = encodeJson "ascending"
  encodeJson Descending = encodeJson "descending"
  encodeJson Ignore = encodeJson "ignore"

-- Field

data Field = Field {
    name :: String
  , aliases :: List String
  , doc :: Maybe String
  , order :: Maybe Order
  , typ :: Type
  -- , default :: (Maybe (Value Type))
  }

derive instance genericAvroField :: Generic Field

instance decodeJsonAvroField :: DecodeJson Field where
  decodeJson json = do
    obj <- jsonObject json
    name <- getField obj "name"
    aliases <- getField obj "aliases"
    doc <- getField obj "doc"
    order <- getField obj "order"
    typ <- getField obj "type"
    pure $ Field { name: name, aliases: aliases, doc: doc, order: order, typ: typ }

instance encodeJsonAvroField :: EncodeJson Field where
  encodeJson (Field field) =
    "name" := field.name
    ~> "aliases" := field.aliases
    ~> "doc" := field.doc
    ~> "order" := field.order
    ~> "type" := field.typ
    ~> jsonEmptyObject