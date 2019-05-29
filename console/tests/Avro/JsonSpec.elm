module Avro.JsonSpec exposing (suite)

import Avro.Json as Json
import Avro.Types as Avro
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import List.Nonempty as NEL
import Test exposing (..)


avroPrims : List ( String, Avro.Type )
avroPrims =
    [ ( "null", Avro.Null )
    , ( "boolean", Avro.Boolean )
    , ( "int", Avro.Int )
    , ( "long", Avro.Long )
    , ( "float", Avro.Float )
    , ( "double", Avro.Double )
    , ( "bytes", Avro.Bytes )
    , ( "string", Avro.String )
    ]


schema : Fuzzer Avro.Type
schema =
    Fuzz.oneOf
        [ Fuzz.constant Avro.Null
        , Fuzz.constant Avro.Boolean
        , Fuzz.constant Avro.Int
        , Fuzz.constant Avro.Long
        , Fuzz.constant Avro.Float
        , Fuzz.constant Avro.Double
        , Fuzz.constant Avro.Bytes
        , Fuzz.constant Avro.String
        ]


suite : Test
suite =
    describe "Avro.Json"
        [ describe "Avro.Json.decodeType" <|
            List.map avroDecodeTest avroPrims
                ++ [ test "should fail to decode an empty string" <|
                        \_ -> Expect.err (decodeString Json.decodeType "")
                   , test "decodes an union" <|
                        \_ ->
                            let
                                expectedUnion =
                                    Avro.Union { options = NEL.cons Avro.Null (NEL.fromElement Avro.Int) }

                                input =
                                    "[\"null\", \"int\"]"
                            in
                            Expect.equal (Ok expectedUnion) (decodeString Json.decodeType input)
                   , test "should fail to decode an empty union" <|
                        \_ -> Expect.err (decodeString Json.decodeType "[]")
                   ]
        , describe "Avro.Json.encodeType" <|
            List.map avroEncodeTest avroPrims
                ++ [ test "encodes an Union" <|
                        \_ ->
                            let
                                unionToEncode =
                                    Avro.Union { options = NEL.cons Avro.Null (NEL.fromElement Avro.Boolean) }
                            in
                            Expect.equal "[\"null\",\"boolean\"]" (encode 0 <| Json.encodeType unionToEncode)
                   ]
        ]


avroDecodeTest : ( String, Avro.Type ) -> Test
avroDecodeTest ( expected, typ ) =
    test ("decodes value '" ++ expected ++ "' string") <|
        \_ -> Expect.equal (Ok typ) (decodeString Json.decodeType <| "\"" ++ expected ++ "\"")


avroEncodeTest : ( String, Avro.Type ) -> Test
avroEncodeTest ( expected, typ ) =
    test ("encodes type " ++ Debug.toString typ) <|
        \_ -> Expect.equal ("\"" ++ expected ++ "\"") (encode 0 <| Json.encodeType typ)
