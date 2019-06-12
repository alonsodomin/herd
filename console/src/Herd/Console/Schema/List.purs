module Herd.Console.Schema.List where

import Prelude

import Data.Array (filter, catMaybes)
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Traversable (traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.MDL.Card as Card
import Halogen.MDL.List as List
import Halogen.MDL.Shadow as Shadow
import Herd.Console.Effect (RemoteAff)
import Herd.Console.Remote as Remote
import Herd.Console.Types (SchemaId(..))
import Herd.Types (SubjectId(..), Version(..))


type SubjectList = Array SchemaId

getSubjectIds :: SubjectList -> Array SubjectId
getSubjectIds = map (\(SchemaId subjectId _) -> subjectId)

filterSubjects :: (SubjectId -> Boolean) -> SubjectList -> SubjectList
filterSubjects pred = filter (\(SchemaId subjectId _) -> pred subjectId)

type State =
  { loading         :: Boolean
  , schemas         :: SubjectList
  , selectedSubject :: Maybe SchemaId
  , subjectFilter   :: Maybe String
  }

data Query a =
    RefreshList a
  | ClickSchema SchemaId a
  | FilterSubjects (Maybe String) a

queryFilterSubjects :: forall a. String -> a -> Query a
queryFilterSubjects str a =
  if Str.length str > 0 then FilterSubjects (Just str) a
    else FilterSubjects Nothing a

type Input = Unit

data Message = SchemaSelected SchemaId

ui :: H.Component HH.HTML Query Input Message RemoteAff
ui =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action RefreshList)
    , finalizer : Nothing
    , receiver: const Nothing
    }

  where
    initialState :: State
    initialState =
      { loading: true
      , schemas: []
      , selectedSubject: Nothing
      , subjectFilter: Nothing
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.classes [ Card.cl.card, Shadow.cl.shadow2dp ] ]
        [ HH.div
          [ HP.class_ Card.cl.cardTitle ]
          [ HH.h2 [ HP.class_ Card.cl.cardTitleText ] [ HH.text "Subjects" ] ]
        , HH.div
          [ HP.class_ Card.cl.cardActions ]
          [ HH.div
            [ HP.classes $ HH.ClassName <$> [ "mdl-textfield", "mdl-js-textfield" ] ]
            [ HH.input
              [ HP.class_ $ HH.ClassName "mdl-textfield__input"
              , HP.type_ HP.InputSearch
              , HE.onValueChange (HE.input queryFilterSubjects)
              ]
            , HH.label
              [ HP.classes $ HH.ClassName <$> [ "mdl-textfield__label" ] ]
              [ HH.i [ HP.class_ $ HH.ClassName "material-icons" ] [ HH.text "search" ] ]
            ]
          ]
        , HH.div
          [ HP.class_ Card.cl.cardSupportingText ]
          [ HH.ul [ HP.class_ List.cl.list ] $ map renderItem visibleSubjects
          ]
        , HH.div
          [ HP.class_ Card.cl.cardMenu ]
          []
        ]
      where visibleSubjects :: SubjectList
            visibleSubjects =
              case state.subjectFilter of
                Nothing -> state.schemas
                Just sf -> filterSubjects (\(SubjectId x) -> sf == x) state.schemas

            renderItem schemaId@(SchemaId (SubjectId subjectId) (Version version)) =
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

    eval :: Query ~> H.ComponentDSL State Query Message RemoteAff
    eval (RefreshList next) = do
      subjectList <- H.lift fetchSubjectList
      H.modify_ (_ { loading = false, schemas = subjectList })
      pure next
    eval (ClickSchema schemaId next) = do
      H.modify_ (_ { selectedSubject = Just $ schemaId })
      H.raise $ SchemaSelected schemaId
      pure next
    eval (FilterSubjects filter next) = do
      H.modify_ (_ { subjectFilter = filter })
      pure next

-- | Fetch the latest versions for all the available subjects

fetchSubjectList :: RemoteAff SubjectList
fetchSubjectList = do
  subjectIds <- Remote.getSubjects
  catMaybes <$> traverse pickSubjectAndVersion subjectIds
  where pickSubjectAndVersion :: SubjectId -> RemoteAff (Maybe SchemaId)
        pickSubjectAndVersion subjectId = do
          maybeVersion <- maximum <$> Remote.getSubjectsBySubjectId subjectId
          case maybeVersion of
            Just v -> pure $ Just (SchemaId subjectId v)
            Nothing -> pure Nothing

