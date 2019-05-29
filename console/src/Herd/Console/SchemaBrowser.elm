module Herd.Console.SchemaBrowser exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Herd.Console.Data.SchemaIndex as SchemaIndex exposing (SchemaIndex)
import Herd.Console.Remote as Remote exposing (AvroSchema, SubjectId, Version)
import Herd.Fetch as Fetch exposing (Fetch(..))
import Html exposing (..)
import Html.Attributes as Html
import Http
import List.Nonempty as NEL exposing (Nonempty)
import Material
import Material.Button as Button
import Material.Drawer.Dismissible as Drawer
import Material.LayoutGrid as LayoutGrid
import Material.List as Lists
import Material.Menu as Menu
import Material.Options as Options exposing (styled)
import Material.TopAppBar as TopAppBar


type Msg
    = GotSubjectIds (Result Http.Error (List SubjectId))
    | GotSchemaVersions SubjectId (Result Http.Error (Maybe (Nonempty Version)))
    | Mdc (Material.Msg Msg)


loadSubjectIds : Cmd Msg
loadSubjectIds =
    Remote.getSubjects GotSubjectIds


loadVersions : SubjectId -> Cmd Msg
loadVersions subjectId =
    Remote.getSubjectsBySubjectId subjectId (\rs -> GotSchemaVersions subjectId (Result.map NEL.fromList rs))


type alias Model =
    { mdc : Material.Model Msg
    , schemaIndex : Fetch SchemaIndex
    , selectedSchema : Maybe (Fetch AvroSchema)
    }


initialModel : Model
initialModel =
    { mdc = Material.defaultModel
    , schemaIndex = Pending
    , selectedSchema = Nothing
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
                Ok (Just versions) ->
                    let
                        list =
                            modelToSchemaList model
                    in
                    ( { model | schemaIndex = Ready (SchemaIndex.insert subjectId versions list) }, Cmd.none )

                Ok Nothing ->
                    ( model, Cmd.none )

                Err err ->
                    ( handleError err model, Cmd.none )

        Mdc m ->
            Material.update Mdc m model



-- View operations


view : Model -> Html Msg
view model =
    viewDrawer model


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
            , styled Html.div [ TopAppBar.fixedAdjust ] [ viewSchemaBrowser model ]
            ]
        ]


viewSchemaBrowser : Model -> Html Msg
viewSchemaBrowser model =
    LayoutGrid.view []
        [ LayoutGrid.cell
            [ LayoutGrid.span4 ]
            [ viewSchemaIndex model ]
        ]


viewSchemaIndex : Model -> Html Msg
viewSchemaIndex model =
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
                [ Lists.graphicIcon [] "code"
                , Lists.text [] [ text subjectId ]
                , Lists.metaText [] <| latestVersion versions
                ]

        latestVersion versions =
            "v" ++ (SchemaIndex.latest versions |> String.fromInt)
    in
    listRender model.schemaIndex
