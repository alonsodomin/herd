module Avro exposing (Schema, jsonDecSchema, jsonEncSchema, toString)

import Avro.Json exposing (..)
import Avro.Types exposing (Type)
import Json.Decode exposing (Decoder)
import Json.Encode as Json


type alias Schema =
    Type


jsonDecSchema : Decoder Schema
jsonDecSchema =
    decodeType


jsonEncSchema : Schema -> Json.Value
jsonEncSchema =
    encodeType


toString : Schema -> String
toString schema =
    Json.encode 4 (encodeType schema)
