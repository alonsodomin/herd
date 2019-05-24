module Herd.Console exposing (..)

import Browser
import Html exposing (Html, table, thead, th, tbody, tr, td, text)
import List
import Dict

import Herd.Console.Model as Model exposing (Model)
import Herd.Console.SchemaList as SchemaList

herdConsole =
  Browser.sandbox { init = Model.init, update = update, view = view }

-- UPDATE

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model = model

-- VIEW

view : Model -> Html Msg
view model = SchemaList.view model.schemas