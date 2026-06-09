{-# LANGUAGE OverloadedStrings #-}

import Test.Cabal.DecodeShowBuildInfo
import Test.Cabal.Prelude

main = cabalTest $ do
  -- the With GHC-9.2+ output contains -this-unit-id
  -- the With GHC-9.4+ output contains -pgmc by default
  skipUnlessGhcVersion ">= 9.4"
  runShowBuildInfo ["exe:Deduplication"] >> withPlan
    ( do
        recordBuildInfo "Deduplication" (exe "Deduplication")
        assertComponent
          "Deduplication"
          (exe "Deduplication")
          defCompAssertion
            { modules = ["Paths_Deduplication"]
            , sourceFiles = ["Main.lhs"]
            , sourceDirs = ["app"]
            }
    )
  runShowBuildInfo ["lib:Deduplication"] >> withPlan
    ( do
        recordBuildInfo "Deduplication" mainLib
        assertComponent
          "Deduplication"
          mainLib
          defCompAssertion
            { modules = ["Other", "Paths_Deduplication"]
            , sourceDirs = ["src"]
            }
    )
