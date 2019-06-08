module Herd.Console.Schema.List where

import Prelude

import Data.Array (catMaybes)
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.MDL.Card as Card
import Halogen.MDL.List as List
import Halogen.MDL.Shadow as Shadow
import Herd.Console.Effect (ConsoleAff)
import Herd.Console.Remote (getSubjects, getSubjectsBySubjectId)
import Herd.Types (SubjectId(..), Version(..))


type SubjectList = Array (Tuple SubjectId Version)

getSubjectIds :: SubjectList -> Array SubjectId
getSubjectIds = map (\(Tuple subjectId _) -> subjectId)

type State =
  { loading         :: Boolean
  , schemas         :: SubjectList
  , selectedSubject :: Maybe (Tuple SubjectId Version)
  }

data Query a =
    Initialize a
  | SubjectSelected SubjectId Version a
  | GetSubjects (Array SubjectId -> a)

type Input = Unit

type Message = Unit

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
      where renderItem (Tuple s@(SubjectId subjectId) v@(Version version)) =
              HH.li
                [ HP.classes [ List.cl.listItem ]
                , HE.onClick (HE.input_ (SubjectSelected s v))
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
    eval (SubjectSelected subjectId version next) = do
      H.modify_ (_ { selectedSubject = Just $ Tuple subjectId version })
      pure next
    eval (GetSubjects reply) = do
      state <- H.get
      pure (reply $ getSubjectIds state.schemas)

-- | Fetch the latest versions for all the available subjects

fetchSubjectList :: ConsoleAff SubjectList
fetchSubjectList = do
  subjectIds <- getSubjects
  catMaybes <$> traverse pickSubjectAndVersion subjectIds
  where pickSubjectAndVersion :: SubjectId -> ConsoleAff (Maybe (Tuple SubjectId Version))
        pickSubjectAndVersion subjectId = do
          maybeVersion <- maximum <$> getSubjectsBySubjectId subjectId
          case maybeVersion of
            Just v -> pure $ Just (Tuple subjectId v)
            Nothing -> pure Nothing

