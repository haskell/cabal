{-# LANGUAGE OverloadedStrings #-}
import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ do
  runShowBuildInfo ["all", "--enable-tests"]
  withPlan $ do
    assertComponent "A" (exe "A")
      defCompAssertion
        { sourceFiles = ["Main.hs"]
        , sourceDirs = ["src"]
        }
    assertComponent "A" mainLib
      defCompAssertion
        { modules = ["A"]
        , sourceDirs = ["src"]
        }

    assertComponent "B" mainLib
      defCompAssertion
        { modules = ["B"]
        , sourceDirs = ["lib"]
        }
    assertComponent "A" (test "A-tests")
      defCompAssertion
        { sourceFiles = ["Test.hs"]
        , sourceDirs = ["src"]
        }
