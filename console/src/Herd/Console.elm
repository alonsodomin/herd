module Herd.Console exposing (..)

import Browser
import Html exposing (Html, table, thead, th, tbody, tr, td, text)
import List
import Dict

import Herd.Console.SchemaList as SchemaList

herdConsole = SchemaList.main