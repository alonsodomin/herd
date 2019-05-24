module Herd.Console.Model exposing (..)

import Herd.Console.SchemaList as SchemaList exposing (SchemaList)

type alias Model =
  { schemas : SchemaList }

init : Model
init = { schemas = SchemaList.empty }