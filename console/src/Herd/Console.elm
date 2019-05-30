module Herd.Console exposing (herdConsole)

import Browser
import Herd.Console.Page.SchemaBrowser as SchemaBrowser
import Html exposing (Html)


herdConsole =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    Tuple.mapBoth SchemaBrowser (Cmd.map GotSchemaBrowserMsg) (SchemaBrowser.init ())


type Model
    = SchemaBrowser SchemaBrowser.Model


type Msg
    = GotSchemaBrowserMsg SchemaBrowser.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotSchemaBrowserMsg subMsg, SchemaBrowser subModel ) ->
            SchemaBrowser.update subMsg subModel
                |> updateWith SchemaBrowser GotSchemaBrowserMsg model


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith liftModel liftMsg model ( subModel, subCmd ) =
    ( liftModel subModel, Cmd.map liftMsg subCmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        SchemaBrowser subModel ->
            SchemaBrowser.view subModel |> Html.map GotSchemaBrowserMsg
