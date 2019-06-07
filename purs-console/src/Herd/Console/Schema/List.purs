module Herd.Console.Schema.List where

import Prelude

import Data.Array (catMaybes)
import Data.Foldable (class Foldable, maximum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herd.Console.Effect (ConsoleAff)
import Herd.Console.Remote (getSubjects, getSubjectsBySubjectId)
import Herd.Types (SubjectId(..), Version(..))


type SubjectList = Array (Tuple SubjectId Version)

getSubjectIds :: SubjectList -> Array SubjectId
getSubjectIds = map (\(Tuple subjectId _) -> subjectId)

type State =
  { loading :: Boolean
  , schemas :: SubjectList
  }

data Query a =
    Initialize a
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
    initialState = { loading: true, schemas: [] }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.ul [] $ map renderItem state.schemas
      where renderItem (Tuple (SubjectId subjectId) (Version version)) =
              HH.li [] [ HH.text subjectId ]

    eval :: Query ~> H.ComponentDSL State Query Message ConsoleAff
    eval (Initialize next) = do
      subjectList <- H.lift fetchSubjectList
      H.modify_ (\st -> st { loading = false, schemas = subjectList })
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
          maybeVersion <- findLatestVersion <$> getSubjectsBySubjectId subjectId
          case maybeVersion of
            Just v -> pure $ Just (Tuple subjectId v)
            Nothing -> pure Nothing

-- | Workaround for missing support of the Ord typeclass in the generated Version type

newtype LatestVersion = LatestVersion Version

derive instance newtypeLatestVersion :: Newtype LatestVersion _
derive instance genericLatestVersion :: Generic LatestVersion _

instance eqLatestVersion :: Eq LatestVersion where
  eq (LatestVersion (Version x)) (LatestVersion (Version y)) = x == y

instance ordLatestVersion :: Ord LatestVersion where
  compare (LatestVersion (Version x)) (LatestVersion (Version y)) = compare x y

instance semigroupLatestVersion :: Semigroup LatestVersion where
  append (LatestVersion (Version x)) (LatestVersion (Version y)) =
    LatestVersion $ Version (max x y)

findLatestVersion :: forall f. Functor f => Foldable f => f Version -> Maybe Version
findLatestVersion vs = unwrap <$> (maximum $ LatestVersion <$> vs)
