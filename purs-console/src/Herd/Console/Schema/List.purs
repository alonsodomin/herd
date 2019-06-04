module Herd.Console.Schema.List where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup.Foldable (class Foldable1, fold1)
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herd.Console.Remote (getSubjects, getSubjectsBySubjectId)
import Herd.Types (SubjectId(..), Version(..))


type State =
  { loading :: Boolean
  , schemas :: Map SubjectId Version
  }

data Query a =
  GetSubjects (Set SubjectId -> a)

type Input = Unit

type Message = Unit

ui :: forall m. H.Component HH.HTML Query Input Message m
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

  where
    initialState :: State
    initialState = { loading: false, schemas: Map.empty }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.ul [] $ map renderItem $ Map.toUnfoldable state.schemas
      where renderItem (Tuple (SubjectId subjectId) (Version version)) =
              HH.li [] [ HH.text subjectId ]

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      GetSubjects reply -> do
        state <- H.get
        pure (reply $ Map.keys state.schemas)

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

latestVersion :: forall f. Functor f => Foldable1 f => f Version -> Version
latestVersion vs = unwrap $ fold1 (LatestVersion <$> vs)