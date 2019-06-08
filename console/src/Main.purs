module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Herd.Console as Console
import Herd.Console.Effect (defaultConsoleSettings, runConsoleAff)

main :: Effect Unit
main = do
  let settings = defaultConsoleSettings
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (H.hoist (runConsoleAff settings) Console.ui) unit body