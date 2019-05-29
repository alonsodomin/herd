module Herd.Console.SchemaBrowser exposing (Model, Msg, init, update, view)

import Avro as Avro
import Dict exposing (Dict)
import Herd.Console.Data.SchemaIndex as SchemaIndex exposing (SchemaIndex)
import Herd.Console.Remote as Remote exposing (AvroSchema, SubjectId, Version)
import Herd.Fetch as Fetch exposing (Fetch)
import Html exposing (..)
import Html.Attributes as Html
import Http
import Json.Encode as Json
import List.Nonempty as NEL exposing (Nonempty)
import Material
import Material.Drawer.Dismissible as Drawer
import Material.LayoutGrid as LayoutGrid
import Material.List as Lists
import Material.Menu as Menu
import Material.Options as Options exposing (styled, when)
import Material.TopAppBar as TopAppBar


type Msg
    = Mdc (Material.Msg Msg)
    | ClickedSchema SubjectId Version
    | ToggleDrawer
    | GotSubjectIds (Result Http.Error (List SubjectId))
    | GotSchemaVersions SubjectId (Result Http.Error (Maybe (Nonempty Version)))
    | GotSchema (Result Http.Error (Maybe AvroSchema))


loadSubjectIds : Cmd Msg
loadSubjectIds =
    Remote.getSubjects GotSubjectIds


loadVersions : SubjectId -> Cmd Msg
loadVersions subjectId =
    Remote.getSubjectsBySubjectId subjectId (\rs -> GotSchemaVersions subjectId (Result.map NEL.fromList rs))


loadSchema : SubjectId -> Version -> Cmd Msg
loadSchema subjectId version =
    Remote.getSubjectsBySubjectIdByVersion subjectId version GotSchema


type alias Model =
    { mdc : Material.Model Msg
    , schemaIndex : Fetch SchemaIndex
    , selectedSchema : Maybe (Fetch AvroSchema)
    , drawerOpen : Bool
    }


initialModel : Model
initialModel =
    { mdc = Material.defaultModel
    , schemaIndex = Fetch.pending
    , selectedSchema = Nothing
    , drawerOpen = True
    }


modelToSchemaList : Model -> SchemaIndex
modelToSchemaList model =
    Fetch.withDefault SchemaIndex.empty model.schemaIndex


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, loadSubjectIds )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSchema subjectId version ->
            ( { model | selectedSchema = Just Fetch.pending }, loadSchema subjectId version )

        ToggleDrawer ->
            ( { model | drawerOpen = not model.drawerOpen }, Cmd.none )

        GotSubjectIds result ->
            case result of
                Ok [] ->
                    ( { model | schemaIndex = Fetch.succeeded SchemaIndex.empty }, Cmd.none )

                Ok subjectIds ->
                    ( model, Cmd.batch (List.map loadVersions subjectIds) )

                Err err ->
                    ( { model | schemaIndex = Fetch.failed err }, Cmd.none )

        GotSchemaVersions subjectId result ->
            case result of
                Ok (Just versions) ->
                    let
                        list =
                            modelToSchemaList model
                    in
                    ( { model | schemaIndex = Fetch.succeeded (SchemaIndex.insert subjectId versions list) }, Cmd.none )

                Ok Nothing ->
                    ( model, Cmd.none )

                Err err ->
                    ( { model | schemaIndex = Fetch.failed err }, Cmd.none )

        GotSchema result ->
            case result of
                Ok (Just schema) ->
                    ( { model | selectedSchema = Just (Fetch.succeeded schema) }, Cmd.none )

                Ok Nothing ->
                    ( { model | selectedSchema = Nothing }, Cmd.none )

                Err err ->
                    ( { model | selectedSchema = Just (Fetch.failed err) }, Cmd.none )

        Mdc m ->
            Material.update Mdc m model



-- View operations


view : Model -> Html Msg
view model =
    Html.div []
        [ viewTopBar model
        , styled Html.div
            [ TopAppBar.fixedAdjust ]
            [ viewDrawer model ]
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
                [ Options.onClick ToggleDrawer ]
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
            [ Drawer.open |> when model.drawerOpen
            , Drawer.onClose ToggleDrawer
            ]
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
            [ viewSchemaBrowser model ]
        ]


viewSchemaBrowser : Model -> Html Msg
viewSchemaBrowser model =
    LayoutGrid.view []
        [ LayoutGrid.cell
            [ LayoutGrid.span4 ]
            [ viewSchemaIndex model ]
        , LayoutGrid.cell
            [ LayoutGrid.span8 ]
            [ viewSelectedSchema model.selectedSchema ]
        ]


viewSchemaIndex : Model -> Html Msg
viewSchemaIndex model =
    let
        listRender =
            Fetch.view <|
                \index ->
                    if SchemaIndex.isEmpty index then
                        text "No schemas found"

                    else
                        Lists.ul Mdc
                            "schema-list"
                            model.mdc
                            [ Lists.avatarList ]
                            (List.map renderSubjectId (Dict.toList index))

        renderSubjectId ( subjectId, versions ) =
            Lists.li [ Options.onClick (ClickedSchema subjectId (SchemaIndex.latest versions)) ]
                [ Lists.graphicIcon [] "code"
                , Lists.text [] [ text subjectId ]
                , Lists.metaText [] <| latestVersionText versions
                ]

        latestVersionText versions =
            "v" ++ (SchemaIndex.latest versions |> String.fromInt)
    in
    listRender model.schemaIndex


viewSelectedSchema : Maybe (Fetch AvroSchema) -> Html Msg
viewSelectedSchema selected =
    case selected of
        Just fetched ->
            Fetch.view (\x -> text <| Avro.toString x) fetched

        Nothing ->
            text "No schema selected"
