module Herd.Console exposing (herdConsole)

import Browser
import Dict
import Herd.Console.SchemaBrowser as SchemaBrowser exposing (Model, Msg)
import Html exposing (Html, table, tbody, td, text, th, thead, tr)
import List


herdConsole =
    Browser.element
        { init = SchemaBrowser.init
        , update = SchemaBrowser.update
        , subscriptions = subscriptions
        , view = SchemaBrowser.view
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
