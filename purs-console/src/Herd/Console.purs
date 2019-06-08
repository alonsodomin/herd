module Herd.Console where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.MDL as MDL
import Halogen.MDL.Layout as Layout
import Halogen.MDL.Navigation as Navigation
import Herd.Console.Effect (ConsoleAff)
import Herd.Console.Page.SchemaBrowser as SchemaBrowser

type State = { }

data Query a =
    InitializeComponent a
  | FinalizeComponent a

data Slot = SchemaBrowserSlot
derive instance eqConsoleSlot :: Eq Slot
derive instance ordConsoleSlot :: Ord Slot

type ConsoleHTML = H.ParentHTML Query SchemaBrowser.Query Slot ConsoleAff

ui :: H.Component HH.HTML Query Unit Void ConsoleAff
ui = H.lifecycleParentComponent
  { initialState: const initialState
  , initializer: Just $ H.action InitializeComponent
  , finalizer: Just $ H.action FinalizeComponent
  , render
  , eval
  , receiver: const Nothing
  }

  where initialState :: State
        initialState = { }

        layoutRef :: H.RefLabel
        layoutRef = H.RefLabel "mdl-layout-ref"

        drawerRef :: H.RefLabel
        drawerRef = H.RefLabel "mdl-layout-drawer-ref"

        render :: State -> ConsoleHTML
        render state =
          HH.div
            [ HP.class_ $ HH.ClassName "root" ]
            [ renderLayout state ]

        renderLayout :: State -> ConsoleHTML
        renderLayout _ =
          HH.div
            [ HP.class_ Layout.cl.layoutContainer ]
            [ HH.div
                [ HP.classes [ Layout.cl.layout, Layout.cl.jsLayout, Layout.cl.layoutFixedHeader ]
                , HP.ref layoutRef
                ]
                [ renderLayoutHeader
                , renderLayoutDrawer
                , renderLayoutContent
                ]
            ]

        renderLayoutHeader :: ConsoleHTML
        renderLayoutHeader =
          HH.header
            [ HP.classes [ Layout.cl.layoutHeader ] ]
            [ HH.div
                [ HP.classes [ Layout.cl.layoutHeaderRow ] ]
                [ HH.span [ HP.classes [ Layout.cl.layoutTitle] ] [ HH.text "Herd Console" ]
                , HH.div [ HP.classes [ Layout.cl.layoutSpacer ] ] []
                , HH.nav [ HP.classes [ Navigation.cl.navigation, Layout.cl.layoutLargeScreenOnly ] ] [ ]
                ]
            ]

        renderLayoutDrawer :: ConsoleHTML
        renderLayoutDrawer =
          HH.div
            [ HP.classes [ Layout.cl.layoutDrawer ]
            , HP.ref drawerRef
            ]
            [ HH.span
                [ HP.classes [ Layout.cl.layoutTitle ] ]
                [ HH.text "Herd Console" ]
            , HH.nav [ HP.classes [ Navigation.cl.navigation ] ] [ ]
            ]

        renderLayoutContent :: ConsoleHTML
        renderLayoutContent =
          HH.main
            [ HP.classes [ Layout.cl.layoutContent ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "page-content" ] ]
                [ HH.slot SchemaBrowserSlot SchemaBrowser.ui unit absurd ]
            ]

        eval :: Query ~> H.ParentDSL State Query SchemaBrowser.Query Slot Void ConsoleAff
        eval (InitializeComponent next) = do
          MDL.upgradeElementByRef layoutRef
          H.put { }  -- required to trigger the rendering of the UI
          pure next
        eval (FinalizeComponent next) = pure next