{-# LANGUAGE OverloadedStrings #-}
import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ do
  runShowBuildInfo ["lib:WithProject"]
  withPlan $ do
    recordBuildInfo "WithProject" mainLib
    assertComponent "WithProject" mainLib defCompAssertion
      { compilerArgsPred = ("-Wall" `elem`) -- added by 'WithProject.cabal'
      , modules = ["Lib"]
      , sourceDirs = ["."]
      }

    -- See #9927
    assertComponent "WithProject" mainLib defCompAssertion
      { compilerArgsPred = ("-haddock" `notElem`) -- added by 'cabal.project', but not present
      , modules = ["Lib"]
      , sourceDirs = ["."]
      }

