{-# LANGUAGE OverloadedStrings #-}

import Test.Cabal.DecodeShowBuildInfo
import Test.Cabal.Prelude

main = do
  cabalTest $ do
    -- the With GHC-9.2+ output contains -this-unit-id
    skipUnlessGhcVersion "^>= 9.2"
    withRepo "repo" $ do
      runShowBuildInfo ["exe:Complex"]
        >> withPlan
          ( do
              recordBuildInfo "Complex" (exe "Complex")
              assertComponent
                "Complex"
                (exe "Complex")
                defCompAssertion
                  { modules = ["Other", "Paths_Complex"]
                  , sourceFiles = ["Main.lhs"]
                  , sourceDirs = ["app"]
                  }
          )

      runShowBuildInfo ["lib:Complex"]
        >> withPlan
          ( do
              recordBuildInfo "Complex" mainLib
              assertComponent
                "Complex"
                mainLib
                defCompAssertion
                  { modules = ["A", "B", "C", "D", "Paths_Complex"]
                  , sourceDirs = ["src", "doesnt-exist"]
                  }
          )

      runShowBuildInfo ["benchmark:complex-benchmarks"]
        >> withPlan
          ( do
              recordBuildInfo "Complex" (bench "complex-benchmarks")
              assertComponent
                "Complex"
                (bench "complex-benchmarks")
                defCompAssertion
                  { modules = ["Paths_Complex"]
                  , sourceFiles = ["Main.hs"]
                  , sourceDirs = ["benchmark"]
                  }
          )

      runShowBuildInfo ["test:func-test"]
        >> withPlan
          ( do
              recordBuildInfo "Complex" (test "func-test")
              assertComponent
                "Complex"
                (test "func-test")
                defCompAssertion
                  { sourceFiles = ["FuncMain.hs"]
                  , sourceDirs = ["test"]
                  }
          )

      runShowBuildInfo ["test:unit-test"]
        >> withPlan
          ( do
              recordBuildInfo "Complex" (test "unit-test")
              assertComponent
                "Complex"
                (test "unit-test")
                defCompAssertion
                  { sourceFiles = ["UnitMain.hs"]
                  , sourceDirs = ["test"]
                  }
          )
