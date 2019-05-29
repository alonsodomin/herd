module Herd.Console exposing (herdConsole)

import Browser
import Dict
import Herd.Console.SchemaList as SchemaList
import Html exposing (Html, table, tbody, td, text, th, thead, tr)
import List


herdConsole =
    Browser.element
        { init = SchemaList.init
        , update = SchemaList.update
        , subscriptions = subscriptions
        , view = SchemaList.view
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
