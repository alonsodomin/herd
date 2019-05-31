module Herd.Avro exposing (Schema, jsonDecSchema, jsonEncSchema)

import Avro as Avro
import Avro.Json exposing (decodeType, encodeType)
import Avro.Types exposing (Type)
import Json.Decode exposing (Decoder)
import Json.Encode as Json

type alias Schema = Avro.Schema

jsonDecSchema : Decoder Schema
jsonDecSchema =
    decodeType


jsonEncSchema : Schema -> Json.Value
jsonEncSchema =
    encodeType