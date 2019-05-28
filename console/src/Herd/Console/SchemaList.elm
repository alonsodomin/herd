module Herd.Console.SchemaList exposing (SchemaList, main, view)

import Browser
import Dict exposing (Dict)
import Herd.Console.Remote as Remote exposing (..)
import Html exposing (Html, table, tbody, td, text, th, thead, tr)
import Http


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias SchemaList =
    Dict SubjectId (List Version)


latestVersion : SubjectId -> SchemaList -> Maybe Version
latestVersion subjectId list =
    Dict.get subjectId list |> Maybe.andThen List.maximum


type Msg
    = GotSubjectIds (Result Http.Error (List SubjectId))
    | GotSchemaVersions SubjectId (Result Http.Error (List Version))


loadSubjectIds : Cmd Msg
loadSubjectIds =
    Remote.getSubjects GotSubjectIds


loadVersions : SubjectId -> Cmd Msg
loadVersions subjectId =
    Remote.getSubjectsBySubjectId subjectId (GotSchemaVersions subjectId)


type Model
    = Initializing
    | Failure String
    | Ready SchemaList


modelToSchemaList : Model -> SchemaList
modelToSchemaList model =
    case model of
        Ready list ->
            list

        _ ->
            Dict.empty


handleError : Http.Error -> Model
handleError err =
    case err of
        Http.BadUrl url ->
            Failure ("Bad url: " ++ url)

        Http.Timeout ->
            Failure "Timed out!"

        Http.NetworkError ->
            Failure "Network error!"

        Http.BadStatus code ->
            Failure ("Bad status code: " ++ (String.fromInt code))

        Http.BadBody msg ->
            Failure ("Bad body: " ++ msg)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Initializing, loadSubjectIds )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSubjectIds result ->
            case result of
                Ok [] ->
                    ( Ready Dict.empty, Cmd.none )

                Ok subjectIds ->
                    ( model, Cmd.batch (List.map loadVersions subjectIds) )

                Err err ->
                    ( handleError err, Cmd.none )

        GotSchemaVersions subjectId result ->
            case result of
                Ok versions ->
                    let
                        list =
                            modelToSchemaList model
                    in
                    ( Ready (Dict.insert subjectId versions list), Cmd.none )

                Err err ->
                    ( handleError err, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View operations


view : Model -> Html msg
view model =
    case model of
        Initializing ->
            text "Loading..."

        Failure m ->
            text m

        Ready ls ->
            viewSchemaList ls


viewSchemaList : SchemaList -> Html msg
viewSchemaList list =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Subject ID" ]
                , th [] [ text "Version" ]
                ]
            ]
        , tbody [] (List.map viewSubjectId (Dict.toList list))
        ]


viewSubjectId : (SubjectId, List Version) -> Html msg
viewSubjectId (subjectId, versions) =
    tr []
        [ td [] [ text subjectId ]
        , td [] [ text ((Maybe.withDefault 0 (List.maximum versions)) |> String.fromInt) ]
        ]
