module Herd.Console.SchemaList exposing (SchemaList, main, view)

import Browser
import Dict exposing (Dict)
import Http
import Herd.Console.Remote as Remote exposing (..)
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
    = GotSubjectIds (Result Http.Error (List SubjectId))
    | GotSchemaVersions SubjectId (Result Http.Error (List Version))

loadSubjectIds : Cmd Msg
loadSubjectIds = Http.send GotSubjectIds <| Remote.getSubjects

loadVersions : SubjectId -> Cmd Msg
loadVersions subjectId = Http.send (GotSchemaVersions subjectId) <| Remote.getSubjectsBySubjectId subjectId

type Model
    = Initializing
    | Failure String
    | Ready SchemaList

modelToSchemaList : Model -> SchemaList
modelToSchemaList model =
    case model of
        Ready list -> list
        _          -> Dict.empty

handleError : Http.Error -> Model
handleError err =
    case err of
        Http.BadUrl url -> Failure ("Bad url: " ++ url)
        Http.Timeout -> Failure "Timed out!"
        Http.NetworkError -> Failure "Network error!"
        Http.BadStatus res -> Failure ("Bad status code: " ++ res.status.message ++ " url: " ++ res.url)
        Http.BadPayload _ _ -> Failure "Bad payload"

init : () -> ( Model, Cmd Msg )
init _ = ( Initializing, loadSubjectIds )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSubjectIds result ->
            case result of
                Ok [] -> (Ready Dict.empty, Cmd.none)
                Ok subjectIds ->
                    (model, Cmd.batch (List.map loadVersions subjectIds))

                Err err -> (handleError err, Cmd.none)

        GotSchemaVersions subjectId result ->
            case result of
                Ok versions ->
                    let list = modelToSchemaList model
                    in (Ready (Dict.insert subjectId versions list), Cmd.none)
                
                Err err -> (handleError err, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View operations


view : Model -> Html msg
view model =
    case model of
        Initializing ->
            text "Loading..."

        Failure m -> text m

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
