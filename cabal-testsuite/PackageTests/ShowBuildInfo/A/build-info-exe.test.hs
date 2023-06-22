{-# LANGUAGE OverloadedStrings #-}

import Test.Cabal.DecodeShowBuildInfo
import Test.Cabal.Prelude

main = cabalTest $ do
  runShowBuildInfo ["exe:A"]
  withPlan $ do
    assertComponent
      "A"
      (exe "A")
      defCompAssertion
        { sourceFiles = ["Main.hs"]
        , sourceDirs = ["src"]
        , -- does not list lib as a target
          compilerArgsPred = all (/= "A-0.1.0.0-inplace")
        }
