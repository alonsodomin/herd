module Herd.Console.Page.SchemaBrowser where

import Prelude

import Data.Const (Const)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Data.Prism (type (\/))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.MDL.Cell as Cell
import Halogen.MDL.Grid as Grid
import Herd.Console.Effect (RemoteAff)
import Herd.Console.Page.SchemaBrowser.Editor as SchemaEditor
import Herd.Console.Page.SchemaBrowser.List as SchemaList
import Herd.Console.Types (SchemaId)

type State =
  { selectedSchema :: Maybe SchemaId }

data Query a =
    HandleListMessage SchemaList.Message a
  | HandleEditorMessage SchemaEditor.Message a

type ChildSlot =
     ListSlot
  \/ EditorSlot
  \/ Void

type ChildQuery =
       SchemaList.Query
  <\/> SchemaEditor.Query
  <\/> Const Void

-- Individual Slots

data ListSlot = ListSlot
derive instance eqListSlot :: Eq ListSlot
derive instance ordListSlot :: Ord ListSlot

cpList :: CP.ChildPath SchemaList.Query ChildQuery ListSlot ChildSlot
cpList = CP.cp1

data EditorSlot = EditorSlot
derive instance eqEditorSlot :: Eq EditorSlot
derive instance ordEditorSlot :: Ord EditorSlot

cpEditor :: CP.ChildPath SchemaEditor.Query ChildQuery EditorSlot ChildSlot
cpEditor = CP.cp2

type BrowserHTML = H.ParentHTML Query ChildQuery ChildSlot RemoteAff
type BrowserDSL = H.ParentDSL State Query ChildQuery ChildSlot Void RemoteAff

ui :: H.Component HH.HTML Query Unit Void RemoteAff
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

  where initialState :: State
        initialState = { selectedSchema: Nothing }

        render :: State -> BrowserHTML
        render state = Grid.el.grid_
          [ Cell.el.cell4Col_ [ HH.slot' cpList ListSlot SchemaList.ui unit (HE.input HandleListMessage) ]
          , Cell.el.cell6Col_ [ HH.slot' cpEditor EditorSlot SchemaEditor.ui state.selectedSchema (HE.input HandleEditorMessage) ]
          ]

        eval :: Query ~> BrowserDSL
        eval (HandleListMessage (SchemaList.SchemaSelected schemaId) a) = do
          H.modify_ (_ { selectedSchema = Just schemaId })
          pure a
        eval (HandleEditorMessage SchemaEditor.SchemaDeleted next) = do
          _ <- H.query' cpList ListSlot $ H.action SchemaList.RefreshList
          pure next