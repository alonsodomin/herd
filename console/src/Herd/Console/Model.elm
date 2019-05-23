module Herd.Console.Model exposing (..)

import Herd.Console.Remote.Types exposing (..)

type alias Model =
  { subjectIds : List SubjectId }

init : Model
init = { subjectIds = [ SubjectId "foo" ] }