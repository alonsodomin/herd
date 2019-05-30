module Herd.Console exposing (herdConsole)

import Browser
import Herd.Console.Page.SchemaBrowser as SchemaBrowser
import Html exposing (Html, h3, h6, text)
import Html.Attributes exposing (href)
import Material
import Material.Drawer.Dismissible as Drawer
import Material.List as Lists
import Material.Options as Options exposing (styled, when)
import Material.TopAppBar as TopAppBar


herdConsole =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Page
    = SchemaBrowser SchemaBrowser.Model


type alias Model =
    { mdc : Material.Model Msg
    , drawerOpen : Bool
    , page : Page
    }


liftPage : (model -> Page) -> Model -> model -> Model
liftPage lift model sub =
    { model | page = lift sub }


liftInitialPage : (model -> Page) -> model -> Model
liftInitialPage lift sub =
    { mdc = Material.defaultModel
    , drawerOpen = True
    , page = lift sub
    }


type Msg
    = Mdc (Material.Msg Msg)
    | ToggleDrawer
    | GotSchemaBrowserMsg SchemaBrowser.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    Tuple.mapBoth (liftInitialPage SchemaBrowser) (Cmd.map GotSchemaBrowserMsg) (SchemaBrowser.init ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( GotSchemaBrowserMsg subMsg, SchemaBrowser subModel ) ->
            SchemaBrowser.update subMsg subModel
                |> updateWith (liftPage SchemaBrowser model) GotSchemaBrowserMsg model

        ( ToggleDrawer, _ ) ->
            ( { model | drawerOpen = not model.drawerOpen }, Cmd.none )

        ( Mdc m, _ ) ->
            Material.update Mdc m model


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith liftModel liftMsg model ( subModel, subCmd ) =
    ( liftModel subModel, Cmd.map liftMsg subCmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View functions


view : Model -> Html Msg
view model =
    let
        viewPage =
            case model.page of
                SchemaBrowser subModel ->
                    SchemaBrowser.view subModel |> Html.map GotSchemaBrowserMsg
    in
    Html.div []
        [ viewTopBar model
        , styled Html.div
            [ TopAppBar.fixedAdjust ]
            [ viewDrawer model viewPage ]
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


viewDrawer : Model -> Html Msg -> Html Msg
viewDrawer model content =
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
                        [ Options.attribute (href "#persistent-drawer")
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
            [ content ]
        ]
