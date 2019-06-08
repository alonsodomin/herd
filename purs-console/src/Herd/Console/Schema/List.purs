module Herd.Console.Schema.List where

import Prelude

import Data.Array (catMaybes)
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.MDL.Card as Card
import Halogen.MDL.List as List
import Halogen.MDL.Shadow as Shadow
import Herd.Console.Effect (ConsoleAff)
import Herd.Console.Remote as Remote
import Herd.Console.Types (SchemaId(..))
import Herd.Types (SubjectId(..), Version(..))


type SubjectList = Array SchemaId

getSubjectIds :: SubjectList -> Array SubjectId
getSubjectIds = map (\(SchemaId subjectId _) -> subjectId)

type State =
  { loading         :: Boolean
  , schemas         :: SubjectList
  , selectedSubject :: Maybe SchemaId
  }

data Query a =
    Initialize a
  | ClickSchema SchemaId a

type Input = Unit

data Message = SchemaSelected SchemaId

ui :: H.Component HH.HTML Query Input Message ConsoleAff
ui =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer : Nothing
    , receiver: const Nothing
    }

  where
    initialState :: State
    initialState = { loading: true, schemas: [], selectedSubject: Nothing }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.classes [ Card.cl.card, Shadow.cl.shadow2dp ] ]
        [ HH.div
          [ HP.class_ Card.cl.cardTitle ]
          [ HH.h2 [ HP.class_ Card.cl.cardTitleText ] [ HH.text "Subjects" ] ]
        , HH.div
          [ HP.class_ Card.cl.cardSupportingText ]
          [ HH.ul [ HP.class_ List.cl.list ] $ map renderItem state.schemas
          ]
        , HH.div
          [ HP.class_ Card.cl.cardMenu ]
          []
        ]
      where renderItem schemaId@(SchemaId (SubjectId subjectId) (Version version)) =
              HH.li
                [ HP.classes [ List.cl.listItem ]
                , HE.onClick (HE.input_ (ClickSchema schemaId))
                ]
                [ HH.span
                  [ HP.class_ List.cl.listItemPrimaryContent ]
                  [ HH.text subjectId ]
                , HH.a
                  [ HP.class_ List.cl.listItemSecondaryAction ]
                  [ HH.text $ "v" <> (show version) ]
                ]                

    eval :: Query ~> H.ComponentDSL State Query Message ConsoleAff
    eval (Initialize next) = do
      subjectList <- H.lift fetchSubjectList
      H.modify_ (_ { loading = false, schemas = subjectList })
      pure next
    eval (ClickSchema schemaId next) = do
      H.modify_ (_ { selectedSubject = Just $ schemaId })
      H.raise $ SchemaSelected schemaId
      pure next

-- | Fetch the latest versions for all the available subjects

fetchSubjectList :: ConsoleAff SubjectList
fetchSubjectList = do
  subjectIds <- Remote.getSubjects
  catMaybes <$> traverse pickSubjectAndVersion subjectIds
  where pickSubjectAndVersion :: SubjectId -> ConsoleAff (Maybe SchemaId)
        pickSubjectAndVersion subjectId = do
          maybeVersion <- maximum <$> Remote.getSubjectsBySubjectId subjectId
          case maybeVersion of
            Just v -> pure $ Just (SchemaId subjectId v)
            Nothing -> pure Nothing

