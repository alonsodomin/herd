module Herd.Console.Route where

import Prelude

data Route =
  SchemaBrowser

derive instance eqConsoleRoute :: Eq Route
instance showConsoleRoute :: Show Route where
  show SchemaBrowser = "Schema Browser"

urlSegment :: Route -> String
urlSegment SchemaBrowser = "/browser"

href :: Route -> String
href route = "#" <> urlSegment route

label :: Route -> String
label = show