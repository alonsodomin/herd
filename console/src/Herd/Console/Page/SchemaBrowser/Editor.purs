module Herd.Console.Page.SchemaBrowser.Editor where

import Prelude

import Data.Argonaut.Encode (encodeJson)
import Data.Avro.Types as Avro
import Data.Const (Const)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff.Class (liftAff)
import Effect.Console as Console
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Data.Prism (type (\/))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.MDL.Button as Button
import Halogen.MDL.Card as Card
import Halogen.MDL.Shadow as Shadow
import Herd.Console.Component.JsonTree as JsonTree
import Herd.Console.Effect (RemoteAff)
import Herd.Console.Remote as Remote
import Herd.Console.Types (SchemaId(..))

type State =
  { selectedSchema :: Maybe (Tuple SchemaId Avro.Type)
  }

data Query a =
    FetchSchema (Maybe SchemaId) a
  | OnNewSchema Button.Message a
  | OnDeleteSchema Button.Message a

type Input = Maybe SchemaId

data Message = SchemaDeleted

type ChildSlot =
     JsonTreeSlot
  \/ ActionSlot
  \/ Void

type ChildQuery =
       JsonTree.Query
  <\/> Button.Query
  <\/> Const Void

data JsonTreeSlot = JsonTreeSlot
derive instance eqEditorJsonTreeSlot :: Eq JsonTreeSlot
derive instance ordEditorJsonTreeSlot :: Ord JsonTreeSlot

cpJsonTree :: CP.ChildPath JsonTree.Query ChildQuery JsonTreeSlot ChildSlot
cpJsonTree = CP.cp1

data ActionSlot =
    NewActionSlot
  | DeleteActionSlot
derive instance eqEditorActionSlot :: Eq ActionSlot
derive instance ordEditorActionSlot :: Ord ActionSlot

cpAction :: CP.ChildPath Button.Query ChildQuery ActionSlot ChildSlot
cpAction = CP.cp2

type EditorHTML = H.ParentHTML Query ChildQuery ChildSlot RemoteAff
type EditorDSL = H.ParentDSL State Query ChildQuery ChildSlot Message RemoteAff

ui :: H.Component HH.HTML Query Input Message RemoteAff
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input FetchSchema
    }

  where initialState :: State
        initialState = { selectedSchema: Nothing }

        render :: State -> EditorHTML
        render state =
          HH.div
            [ HP.id_ "schema-editor"
            , HP.classes [ Card.cl.card, Shadow.cl.shadow2dp ]
            ]
            [ HH.div
              [ HP.class_ Card.cl.cardTitle ]
              [ HH.h2 [ HP.class_ Card.cl.cardTitleText ] [ HH.text "Schema" ] ]
            , HH.div
              [ HP.class_ Card.cl.cardActions ]
              [ displayActions ]
            , HH.div
              [ HP.class_ Card.cl.cardSupportingText ]
              [ displaySchema $ snd <$> state.selectedSchema
              ]
            , HH.div
              [ HP.class_ Card.cl.cardMenu ]
              []
            ]
          where displayActions :: EditorHTML
                displayActions = HH.div_ [ newSchemaButton, deleteSchemaButton ]

                newSchemaButton :: EditorHTML
                newSchemaButton =
                  HH.slot'
                    cpAction
                    NewActionSlot
                    (H.hoist liftAff Button.button)
                    (Button.init
                      { type: Button.Raised
                      , color: Button.Colored
                      , content: Button.Text "New"
                      , disabled: false
                      , ripple: true
                      }
                    )
                    (HE.input OnNewSchema)

                deleteSchemaButton :: EditorHTML
                deleteSchemaButton =
                  HH.slot'
                    cpAction
                    DeleteActionSlot
                    (H.hoist liftAff Button.button)
                    (Button.init
                        { type: Button.Raised
                        , color: Button.Colored
                        , content: Button.Text "Delete"
                        , disabled: isNothing state.selectedSchema
                        , ripple: true
                        })
                    (HE.input OnDeleteSchema)
          
                displaySchema :: Maybe Avro.Type -> EditorHTML
                displaySchema (Just schema) =
                  HH.slot' cpJsonTree JsonTreeSlot JsonTree.component (encodeJson schema) absurd
                displaySchema Nothing = HH.span_ [ HH.text "No schema selected" ]

        eval :: Query ~> EditorDSL
        eval (FetchSchema maybeSchemaId next) = do
          case maybeSchemaId of
            Just schemaId -> do
              H.liftEffect $ Console.log ("Schema '" <> (show schemaId) <> "' has been selected")
              maybeSchema <- H.lift $ fetchSchema schemaId
              H.modify_ (_ { selectedSchema = (Tuple schemaId) <$> maybeSchema })
              pure next
            Nothing -> pure next
        eval (OnNewSchema _ next) = do
          H.liftEffect $ Console.log "Opening editor for creating a new schema"
          pure next
        eval (OnDeleteSchema _ next) = do
          state <- H.get
          maybeSchemaId <- pure $ fst <$> state.selectedSchema
          case maybeSchemaId of
            Just (SchemaId subjectId version) -> do
              H.liftEffect $ Console.log ("Deleting schema: " <> (unwrap subjectId))
              H.lift $ Remote.deleteSubjectsBySubjectIdByVersion subjectId version
              H.modify_ (_ { selectedSchema = Nothing })
              H.raise SchemaDeleted
              pure next
            Nothing -> do
              H.liftEffect $ Console.log "Either no schema is selected or the button is disabled"
              pure next

-- | Fetch the schema definition
fetchSchema :: SchemaId -> RemoteAff (Maybe Avro.Type)
fetchSchema (SchemaId subjectId version) =
  Remote.getSubjectsBySubjectIdByVersion subjectId version
