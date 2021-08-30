{-# LANGUAGE OverloadedStrings #-}
import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo
import           Test.Cabal.Plan
import           Control.Monad.Trans.Reader

main = cabalTest $ do
  runShowBuildInfo ["exe:A"]
  withPlan $ do
    assertComponent "A" (exe "A")
      defCompAssertion
          { sourceFiles = ["Main.hs"]
          , sourceDirs = ["src"]
          -- does not list lib as a target
          , compilerArgsPred = all (/= "A-0.1.0.0-inplace")
          }

  cabal' "v2-build" ["exe:A", "--disable-build-info"]
  withPlan $ do
    Just plan <- fmap testPlan ask
    let fp = buildInfoFile plan "A" (exe "A")
    shouldNotExist fp
