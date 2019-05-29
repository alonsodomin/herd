module Herd.Console.SchemaList exposing (init, update, view)

import Dict exposing (Dict)
import Herd.Console.Data.SchemaIndex as SchemaIndex exposing (SchemaIndex)
import Herd.Console.Remote as Remote exposing (SubjectId, Version)
import Herd.Fetch as Fetch exposing (Fetch(..))
import Html exposing (..)
import Html.Attributes as Html
import Http
import Material
import Material.Button as Button
import Material.Drawer.Dismissible as Drawer
import Material.List as Lists
import Material.Menu as Menu
import Material.Options as Options exposing (styled)
import Material.TopAppBar as TopAppBar


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


type alias Model =
    { mdc : Material.Model Msg
    , schemaIndex : Fetch SchemaIndex
    }


initialModel : Model
initialModel =
    { mdc = Material.defaultModel
    , schemaIndex = Pending
    }


modelToSchemaList : Model -> SchemaIndex
modelToSchemaList model =
    case model.schemaIndex of
        Ready list ->
            list

        _ ->
            SchemaIndex.empty


handleError : Http.Error -> Model -> Model
handleError err model =
    case err of
        Http.BadUrl url ->
            { model | schemaIndex = Failed ("Bad url: " ++ url) }

        Http.Timeout ->
            { model | schemaIndex = Failed "Timed out!" }

        Http.NetworkError ->
            { model | schemaIndex = Failed "Network error!" }

        Http.BadStatus code ->
            { model | schemaIndex = Failed ("Bad status code: " ++ String.fromInt code) }

        Http.BadBody msg ->
            { model | schemaIndex = Failed ("Bad body: " ++ msg) }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, loadSubjectIds )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSubjectIds result ->
            case result of
                Ok [] ->
                    ( { model | schemaIndex = Ready Dict.empty }, Cmd.none )

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
                    ( { model | schemaIndex = Ready (Dict.insert subjectId versions list) }, Cmd.none )

                Err err ->
                    ( handleError err model, Cmd.none )

        Mdc m ->
            Material.update Mdc m model



-- View operations


view : Model -> Html Msg
view model =
    viewDrawer model


viewDrawer : Model -> Html Msg
viewDrawer model =
    Html.div []
        [ Drawer.view Mdc
            "herd-drawer"
            model.mdc
            []
            [ Drawer.header []
                [ styled h3 [ Drawer.title ] [ text "Herd" ]
                , styled h6 [ Drawer.subTitle ] [ text "username" ]
                ]
            , Drawer.content []
                [ Lists.nav Mdc
                    "herd-nav"
                    model.mdc
                    []
                    [ Lists.a
                        [ Options.attribute (Html.href "#persistent-drawer")
                        , Lists.activated
                        ]
                        [ Lists.graphicIcon [] "inbox"
                        , text "Schemas"
                        ]
                    ]
                ]
            ]
        , styled Html.div
            [ Drawer.appContent ]
            [ viewTopBar model
            , styled Html.div [ TopAppBar.fixedAdjust ] [ viewSchemaList model ]
            ]
        ]


viewTopBar : Model -> Html Msg
viewTopBar model =
    TopAppBar.view Mdc
        "herd-topbar"
        model.mdc
        [ TopAppBar.fixed ]
        [ TopAppBar.section [ TopAppBar.alignStart ]
            [ TopAppBar.navigationIcon Mdc
                "burger-menu"
                model.mdc
                []
                -- [ Options.onClick OpenDrawer ]
                "menu"
            , TopAppBar.title [] [ text "Herd" ]
            ]
        ]


viewSchemaList : Model -> Html Msg
viewSchemaList model =
    let
        listRender =
            Fetch.view <|
                \list ->
                    Lists.ul Mdc
                        "schema-list"
                        model.mdc
                        [ Lists.avatarList ]
                        (List.map renderSubjectId (Dict.toList list))

        renderSubjectId ( subjectId, versions ) =
            Lists.li []
                [ Lists.graphicIcon [] "subject"
                , Lists.text [] [ text subjectId ]
                , Lists.metaText [] ("v" ++ (Maybe.withDefault 0 (List.maximum versions) |> String.fromInt))
                ]
    in
    listRender model.schemaIndex
