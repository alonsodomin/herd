module Herd.Console exposing (..)

import Browser
import Html exposing (Html, table, thead, th, tbody, tr, td, text)
import List

import Herd.Console.Model as Model exposing (Model)
import Herd.Console.Remote.Types exposing (..)

herdConsole =
  Browser.sandbox { init = Model.init, update = update, view = view }

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