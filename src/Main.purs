module Main where

import Prelude

import Effect (Effect)
import UI (ui)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = do
  runHalogenAff do
    body <- awaitBody
    runUI ui unit body
