module Herd.Console.SchemaList exposing (SchemaList, main, view)

import Browser
import Dict exposing (Dict)
import Herd.Console.Remote as Remote exposing (SubjectId, Version)
import Html exposing (Html, table, tbody, td, text, th, thead, tr)
import Http
import Material
import Material.Button as Button
import Material.Menu as Menu
import Material.Options exposing (cs, css, styled)


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
    | Mdc (Material.Msg Msg)


loadSubjectIds : Cmd Msg
loadSubjectIds =
    Remote.getSubjects GotSubjectIds


loadVersions : SubjectId -> Cmd Msg
loadVersions subjectId =
    Remote.getSubjectsBySubjectId subjectId (GotSchemaVersions subjectId)


type Status a
    = Loading
    | Failed String
    | Ready a


type alias Model =
    { mdc : Material.Model Msg
    , schemaList : Status SchemaList
    }


initialModel : Model
initialModel =
    { mdc = Material.defaultModel
    , schemaList = Loading
    }


modelToSchemaList : Model -> SchemaList
modelToSchemaList model =
    case model.schemaList of
        Ready list ->
            list

        _ ->
            Dict.empty


handleError : Http.Error -> Model -> Model
handleError err model =
    case err of
        Http.BadUrl url ->
            { model | schemaList = Failed ("Bad url: " ++ url) }

        Http.Timeout ->
            { model | schemaList = Failed "Timed out!" }

        Http.NetworkError ->
            { model | schemaList = Failed "Network error!" }

        Http.BadStatus code ->
            { model | schemaList = Failed ("Bad status code: " ++ String.fromInt code) }

        Http.BadBody msg ->
            { model | schemaList = Failed ("Bad body: " ++ msg) }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, loadSubjectIds )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSubjectIds result ->
            case result of
                Ok [] ->
                    ( { model | schemaList = Ready Dict.empty }, Cmd.none )

                Ok subjectIds ->
                    ( model, Cmd.batch (List.map loadVersions subjectIds) )

                Err err ->
                    ( handleError err model, Cmd.none )

        GotSchemaVersions subjectId result ->
            case result of
                Ok versions ->
                    let
                        list =
                            modelToSchemaList model
                    in
                    ( { model | schemaList = Ready (Dict.insert subjectId versions list) }, Cmd.none )

                Err err ->
                    ( handleError err model, Cmd.none )

        Mdc m ->
            Material.update Mdc m model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View operations


view : Model -> Html Msg
view model =
    case model.schemaList of
        Loading ->
            Html.div [] [ viewMenu model, text "Loading..." ]

        Failed m ->
            text m

        Ready ls ->
            Html.div [] [ viewMenu model, viewSchemaList ls ]


viewMenu : Model -> Html Msg
viewMenu model =
    styled Html.div
        [ Menu.surfaceAnchor ]
        [ Button.view Mdc
            "my-button"
            model.mdc
            [ Menu.attach Mdc "my-menu" ]
            [ text "Show" ]
        , Menu.view Mdc
            "my-menu"
            model.mdc
            []
            (Menu.ul []
                [ Menu.li [] [ text "Menu Item 1" ]
                , Menu.li [] [ text "Menu Item 2" ]
                ]
            )
        ]


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


viewSubjectId : ( SubjectId, List Version ) -> Html msg
viewSubjectId ( subjectId, versions ) =
    tr []
        [ td [] [ text subjectId ]
        , td [] [ text (Maybe.withDefault 0 (List.maximum versions) |> String.fromInt) ]
        ]
