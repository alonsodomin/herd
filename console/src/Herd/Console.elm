module Herd.Console exposing (..)

import Browser
import Html exposing (Html, table, thead, th, tbody, tr, td, text)
import List

import Herd.Console.Remote.API exposing (..)

type alias Model =
  { subjectIds : List SubjectId }

herdConsole =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

init : Model
init = { subjectIds = [ SubjectId "foo" ] }

-- UPDATE

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model = model

-- VIEW

viewSubjectId : SubjectId -> Html Msg
viewSubjectId (SubjectId subjectId) = tr [] [
    td [] [ text subjectId ]
  ]

view : Model -> Html Msg
view model =
  table [] [
    thead [] [
      tr [] [
        th [] [ text "Subject ID" ]
      ]
    ]
  , tbody [] (List.map viewSubjectId model.subjectIds)
  ]