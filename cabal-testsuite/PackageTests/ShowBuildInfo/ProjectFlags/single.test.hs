{-# LANGUAGE OverloadedStrings #-}

import Test.Cabal.DecodeShowBuildInfo
import Test.Cabal.Prelude

main = cabalTest $ do
  -- the With GHC-9.2+ output contains -this-unit-id
  -- the With GHC-9.4+ output contains -pgmc by default
  skipUnlessGhcVersion ">= 9.4"
  runShowBuildInfo ["exe:ProjectFlags"] >> withPlan
    ( do
        recordBuildInfo "ProjectFlags" (exe "ProjectFlags")
        assertComponent
          "ProjectFlags"
          (exe "ProjectFlags")
          defCompAssertion
            { modules = ["Paths_ProjectFlags"]
            , sourceFiles = ["Main.lhs"]
            , sourceDirs = ["app"]
            }
    )
  runShowBuildInfo ["lib:ProjectFlags"] >> withPlan
    ( do
        recordBuildInfo "ProjectFlags" mainLib
        assertComponent
          "ProjectFlags"
          mainLib
          defCompAssertion
            { modules = ["Other", "Paths_ProjectFlags"]
            , sourceDirs = ["src"]
            }
    )
