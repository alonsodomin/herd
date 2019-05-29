module Avro.Json exposing (decodeType, encodeType)

import Array
import Avro.Types as Type exposing (Type)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Json
import List.Nonempty as NEL exposing (Nonempty)


encodeType : Type -> Json.Value
encodeType typ =
    case typ of
        Type.Null ->
            Json.string "null"

        Type.Boolean ->
            Json.string "boolean"

        Type.Int ->
            Json.string "int"

        Type.Union { options } ->
            Json.array encodeType (Array.fromList (NEL.toList options))


decodeType : Decoder Type
decodeType =
    Decode.oneOf
        [ decodeNull
        , decodeBoolean
        , decodeInt
        , Decode.lazy (\_ -> decodeUnion)
        ]


match : a -> ( String, Type ) -> a -> Decoder Type
match toMatch ( err, res ) value =
    if value == toMatch then
        Decode.succeed res

    else
        Decode.fail err


decodeFromString : (String -> Decoder Type) -> Decoder Type
decodeFromString f =
    Decode.string |> Decode.andThen f


decodeNull : Decoder Type
decodeNull =
    decodeFromString <| match "null" ( "Not a null", Type.Null )


decodeBoolean : Decoder Type
decodeBoolean =
    decodeFromString <| match "boolean" ( "Not a boolean", Type.Boolean )


decodeInt : Decoder Type
decodeInt =
    decodeFromString <| match "int" ( "Not an int", Type.Int )


decodeUnion : Decoder Type
decodeUnion =
    let
        handleParsed parsed =
            case parsed of
                Just opts ->
                    Decode.succeed <| Type.Union { options = opts }

                Nothing ->
                    Decode.fail "Empty union"
    in
    Decode.array decodeType
        |> Decode.map Array.toList
        |> Decode.map NEL.fromList
        |> Decode.andThen handleParsed
