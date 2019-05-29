module Avro.Json exposing (decodeType, encodeType)

import Avro.Types exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Json


encodeType : Type -> Json.Value
encodeType typ =
    case typ of
        Null ->
            Json.string "null"

        Boolean ->
            Json.string "boolean"

        Int ->
            Json.string "int"


decodeType : Decoder Type
decodeType =
    Decode.oneOf [ decodeNull, decodeBoolean, decodeInt ]


decodeNull : Decoder Type
decodeNull =
    let
        succeedIfNull value =
            if value == "null" then
                Decode.succeed Null

            else
                Decode.fail "Not a null"
    in
    Decode.string |> Decode.andThen succeedIfNull


decodeBoolean : Decoder Type
decodeBoolean =
    let
        succeedIfBoolean value =
            if value == "boolean" then
                Decode.succeed Boolean

            else
                Decode.fail "Not a boolean"
    in
    Decode.string |> Decode.andThen succeedIfBoolean


decodeInt : Decoder Type
decodeInt =
    let
        succeedIfInt value =
            if value == "int" then
                Decode.succeed Int

            else
                Decode.fail "Not an int"
    in
    Decode.string |> Decode.andThen succeedIfInt
