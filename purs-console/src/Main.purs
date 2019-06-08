module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Herd.Console.Effect (defaultConsoleSettings, runConsoleAff)
import Herd.Console.Page.SchemaBrowser as SchemaBrowser

main :: Effect Unit
main = do
  let settings = defaultConsoleSettings
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (H.hoist (runConsoleAff settings) SchemaBrowser.ui) unit body