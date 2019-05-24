module Herd.Console.SchemaList exposing (SchemaList, empty, view)

import Html exposing (Html, table, thead, th, tbody, tr, td, text)
import Dict exposing (Dict)

import Herd.Console.Remote.Types exposing (..)

type alias SchemaList = Dict SubjectId (List Version)

empty : SchemaList
empty = Dict.empty

view : SchemaList -> Html msg
view reg = table [] [
    thead [] [
      tr [] [
        th [] [ text "Subject ID" ]
      ]
    ]
  , tbody [] (List.map viewSubjectId (Dict.keys reg))
  ]

viewSubjectId : SubjectId -> Html msg
viewSubjectId (SubjectId subjectId) = tr [] [
    td [] [ text subjectId ]
  ]