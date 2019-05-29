module Avro exposing (Schema, jsonEncSchema, jsonDecSchema, toString)

import Json.Encode as Json
import Json.Decode exposing (Decoder)

type alias Schema = Json.Value

jsonDecSchema : Decoder Schema
jsonDecSchema = Json.Decode.value

jsonEncSchema : Schema -> Json.Value
jsonEncSchema = identity

toString : Schema -> String
toString = Json.encode 0