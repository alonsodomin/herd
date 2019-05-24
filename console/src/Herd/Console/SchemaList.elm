module Herd.Console.SchemaList exposing (SchemaList, main, view)

import Browser
import Dict exposing (Dict)
import Herd.Console.Remote exposing (..)
import Html exposing (Html, table, tbody, td, text, th, thead, tr)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias SchemaList =
    Dict SubjectId (List Version)


type Msg
    = GotSchemaList SchemaList


type Model
    = Initializing
    | Ready SchemaList


init : () -> ( Model, Cmd Msg )
init _ =
    let
        loadSchemaList =
            Cmd.none
    in
    ( Initializing, loadSchemaList )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSchemaList list ->
            ( Ready list, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View operations


view : Model -> Html msg
view model =
    case model of
        Initializing ->
            text ""

        Ready ls ->
            viewSchemaList ls


viewSchemaList : SchemaList -> Html msg
viewSchemaList list =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Subject ID" ]
                ]
            ]
        , tbody [] (List.map viewSubjectId (Dict.keys list))
        ]


viewSubjectId : SubjectId -> Html msg
viewSubjectId subjectId =
    tr []
        [ td [] [ text subjectId ]
        ]
