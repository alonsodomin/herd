module Herd.Console.Schema.Editor where

import Prelude

import Data.Argonaut.Encode (encodeJson)
import Data.Avro.Types as Avro
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.MDL.Card as Card
import Halogen.MDL.Shadow as Shadow

import Herd.Console.Component.JsonTree as JsonTree
import Herd.Console.Effect (ConsoleAff)
import Herd.Console.Remote as Remote
import Herd.Console.Types (SchemaId(..))

type State =
  { selectedSchema :: Maybe Avro.Type
  }

data Query a =
    FetchSchema (Maybe SchemaId) a
  | GetSchema (Maybe Avro.Type -> a)

type Input = Maybe SchemaId

type Message = Void

data Slot = JsonTreeSlot
derive instance eqEditorSlot :: Eq Slot
derive instance ordEditorSlot :: Ord Slot

type EditorHTML = H.ParentHTML Query JsonTree.Query Slot ConsoleAff
type EditorDSL = H.ParentDSL State Query JsonTree.Query Slot Void ConsoleAff

ui :: H.Component HH.HTML Query Input Message ConsoleAff
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
              [ HP.class_ Card.cl.cardSupportingText ]
              [ displaySchema state.selectedSchema
              ]
            , HH.div
              [ HP.class_ Card.cl.cardMenu ]
              []
            ]
          where displaySchema :: Maybe Avro.Type -> EditorHTML
                displaySchema (Just schema) =
                  HH.slot JsonTreeSlot JsonTree.component (encodeJson schema) absurd
                displaySchema Nothing = HH.span_ [ HH.text "No schema selected" ]

        eval :: Query ~> EditorDSL
        eval (FetchSchema maybeSchemaId next) = do
          case maybeSchemaId of
            Just schemaId -> do
              schema <- H.lift $ fetchSchema schemaId
              H.modify_ (_ { selectedSchema = schema })
              pure next
            Nothing -> pure next
        eval (GetSchema reply) = do
          state <- H.get
          pure $ reply state.selectedSchema

-- | Fetch the schema definition
fetchSchema :: SchemaId -> ConsoleAff (Maybe Avro.Type)
fetchSchema (SchemaId subjectId version) =
  Remote.getSubjectsBySubjectIdByVersion subjectId version