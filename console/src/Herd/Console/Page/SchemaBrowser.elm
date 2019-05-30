module Herd.Console.Page.SchemaBrowser exposing (Model, Msg, init, update, view)

import Avro as Avro
import Dict exposing (Dict)
import Herd.Console.Data.SchemaIndex as SchemaIndex exposing (SchemaIndex)
import Herd.Console.Remote as Remote exposing (AvroSchema, SubjectId, Version)
import Herd.Fetch as Fetch exposing (Fetch)
import Html exposing (..)
import Http
import Json.Encode as Json
import List.Nonempty as NEL exposing (Nonempty)
import Material
import Material.Elevation as Elevation
import Material.LayoutGrid as LayoutGrid
import Material.List as Lists
import Material.Menu as Menu
import Material.Options as Options exposing (cs, css, styled, when)
import Material.TextField as TextField
import Material.Typography as Typography



-- Model


type alias Model =
    { mdc : Material.Model Msg
    , schemaIndex : Fetch SchemaIndex
    , selectedSchema : Maybe (Fetch AvroSchema)
    , filterBySubject : Maybe SubjectId
    }


initialModel : Model
initialModel =
    { mdc = Material.defaultModel
    , schemaIndex = Fetch.pending
    , selectedSchema = Nothing
    , filterBySubject = Nothing
    }


modelToSchemaList : Model -> SchemaIndex
modelToSchemaList model =
    Fetch.withDefault SchemaIndex.empty model.schemaIndex



-- Messages


type Msg
    = Mdc (Material.Msg Msg)
    | ClickedSchema SubjectId Version
    | FilterBySubject String
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



-- Implementation


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, loadSubjectIds )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSchema subjectId version ->
            ( { model | selectedSchema = Just Fetch.pending }, loadSchema subjectId version )

        FilterBySubject str ->
            let
                maybeFilter =
                    if String.length str > 0 then
                        Just str

                    else
                        Nothing
            in
            ( { model | filterBySubject = maybeFilter }, Cmd.none )

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
        indexToRender =
            case model.filterBySubject of
                Just searchTerm ->
                    Fetch.map (SchemaIndex.filter (\subjectId -> String.contains searchTerm subjectId)) model.schemaIndex

                Nothing ->
                    model.schemaIndex

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

        renderSearchBox =
            TextField.view Mdc
                "subject-filter"
                model.mdc
                [ TextField.label "Search"
                , Options.onInput FilterBySubject
                , css "width" "100%"
                ]
                []

        latestVersionText versions =
            "v" ++ (SchemaIndex.latest versions |> String.fromInt)
    in
    styled Html.div
        [ Elevation.z2 ]
        [ Html.div [] [ styled h2 [ Typography.title ] [ text "Subjects" ] ]
        , renderSearchBox
        , listRender indexToRender
        ]


viewSelectedSchema : Maybe (Fetch AvroSchema) -> Html Msg
viewSelectedSchema selected =
    let
        renderSchema schema =
            styled Html.div
                [ Elevation.z2 ]
                [ styled h2 [ Typography.title ] [ text "Schema" ]
                , styled Html.pre style [ text (Avro.prettyPrint schema) ]
                ]

        style =
            [ css "font" sourceCodeFont, css "background-color" "white" ]

        sourceCodeFont =
            "12px/normal 'Monaco', 'Menlo', 'Ubuntu Mono', 'Consolas', 'source-code-pro', monospace"
    in
    case selected of
        Just fetched ->
            Fetch.view renderSchema fetched

        Nothing ->
            text "No schema selected"
