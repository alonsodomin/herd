module Herd.Console.Schema.List where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Herd.Types (SubjectId(..), Version(..))

type State = Map SubjectId Version

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
    initialState = Map.empty

    render :: State -> H.ComponentHTML Query
    render state =
      HH.ul [] $ map renderItem $ Map.toUnfoldable state
      where renderItem (Tuple (SubjectId subjectId) (Version version)) =
              HH.li [] [ HH.text subjectId ]

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      GetSubjects reply -> do
        state <- H.get
        pure (reply $ Map.keys state)