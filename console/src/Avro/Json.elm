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

        Type.Long ->
            Json.string "long"

        Type.Float ->
            Json.string "float"

        Type.Double ->
            Json.string "double"

        Type.Bytes ->
            Json.string "bytes"

        Type.String ->
            Json.string "string"

        Type.Array { items } ->
            Json.object [ ( "type", Json.string "array" ), ( "items", encodeType items ) ]

        Type.Map { values } ->
            Json.object [ ( "type", Json.string "map" ), ( "values", encodeType values ) ]

        Type.Union { options } ->
            Json.array encodeType (Array.fromList (NEL.toList options))


decodeType : Decoder Type
decodeType =
    Decode.oneOf
        [ decodeNull
        , decodeBoolean
        , decodeInt
        , decodeLong
        , decodeFloat
        , decodeDouble
        , decodeBytes
        , decodeString
        , decodeArray
        , decodeMap
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


decodeLong : Decoder Type
decodeLong =
    decodeFromString <| match "long" ( "Not a long", Type.Long )


decodeFloat : Decoder Type
decodeFloat =
    decodeFromString <| match "float" ( "Not a float", Type.Float )


decodeDouble : Decoder Type
decodeDouble =
    decodeFromString <| match "double" ( "Not a double", Type.Double )


decodeBytes : Decoder Type
decodeBytes =
    decodeFromString <| match "bytes" ( "Not bytes", Type.Bytes )


decodeString : Decoder Type
decodeString =
    decodeFromString <| match "string" ( "Not a string", Type.String )


decodeComplex : String -> String -> Decoder Type -> Decoder Type
decodeComplex typeName errMsg decoder =
    let
        handleTypeName typ =
            if typ == typeName then
                decoder

            else
                Decode.fail errMsg
    in
    Decode.field "type" Decode.string |> Decode.andThen handleTypeName


decodeArray : Decoder Type
decodeArray =
    decodeComplex "array" "Not an array" <|
        Decode.lazy (\_ -> Decode.field "items" decodeType |> Decode.map (\x -> Type.Array { items = x }))


decodeMap : Decoder Type
decodeMap =
    decodeComplex "map" "Not a map" <|
        Decode.lazy (\_ -> Decode.field "values" decodeType |> Decode.map (\x -> Type.Map { values = x }))


decodeUnion : Decoder Type
decodeUnion =
    let
        handleParsed parsed =
            case parsed of
                Just opts ->
                    Decode.succeed <| Type.Union { options = opts }

                Nothing ->
                    Decode.fail "Empty union!"
    in
    Decode.array decodeType
        |> Decode.map Array.toList
        |> Decode.map NEL.fromList
        |> Decode.andThen handleParsed
