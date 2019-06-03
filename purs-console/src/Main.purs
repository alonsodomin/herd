module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Herd.Console.Schema.List as SchemaList

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI SchemaList.ui unit body