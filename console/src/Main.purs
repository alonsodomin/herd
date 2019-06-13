module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Herd.Console as Console
import Herd.Console.Effect (defaultRemoteSettings, runRemoteAff)

main :: Effect Unit
main = do
  let settings = defaultRemoteSettings
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (H.hoist (runRemoteAff settings) Console.ui) unit body