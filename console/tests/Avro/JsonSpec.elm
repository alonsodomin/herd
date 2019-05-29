module Avro.JsonSpec exposing (suite)

import Avro.Json as Json
import Avro.Types as Avro
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import List.Nonempty as NEL
import Test exposing (..)


suite : Test
suite =
    describe "Avro.Json"
        [ describe "Avro.Json.decodeType"
            [ test "should fail to decode an empty string" <|
                \_ -> Expect.err (decodeString Json.decodeType "")
            , test "decodes a quoted 'null' string" <|
                \_ -> Expect.equal (Ok Avro.Null) (decodeString Json.decodeType "\"null\"")
            , test "decodes quoted 'boolean' string" <|
                \_ -> Expect.equal (Ok Avro.Boolean) (decodeString Json.decodeType "\"boolean\"")
            , test "decodes quoted 'int' string" <|
                \_ -> Expect.equal (Ok Avro.Int) (decodeString Json.decodeType "\"int\"")
            , test "decodes an union" <|
                \_ ->
                    let
                        expectedUnion =
                            Avro.Union { options = NEL.cons Avro.Null (NEL.fromElement Avro.Int) }

                        input =
                            "[\"null\", \"int\"]"
                    in
                    Expect.equal (Ok expectedUnion) (decodeString Json.decodeType input)
            ]
        , describe "Avro.Json.encodeType"
            [ test "encodes a Null" <|
                \_ -> Expect.equal "\"null\"" (encode 0 <| Json.encodeType Avro.Null)
            , test "encodes a Boolean" <|
                \_ -> Expect.equal "\"boolean\"" (encode 0 <| Json.encodeType Avro.Boolean)
            , test "encodes an Int" <|
                \_ -> Expect.equal "\"int\"" (encode 0 <| Json.encodeType Avro.Int)
            , test "encodes an Union" <|
                \_ ->
                    let
                        unionToEncode =
                            Avro.Union { options = NEL.cons Avro.Null (NEL.fromElement Avro.Boolean) }
                    in
                    Expect.equal "[\"null\",\"boolean\"]" (encode 0 <| Json.encodeType unionToEncode)
            ]
        ]
