import           Test.Cabal.Prelude

main = cabalTest $ do
    r <- fails $ cabal' "show-build-info" ["exe:B", "-v1"]
    assertOutputContains "Internal error in target matching." r

    r <- fails $ cabal' "show-build-info" ["C", "-v1"]
    assertOutputContains "Cannot show-build-info the package C, it is not in this project (either directly or indirectly)." r

    r <- fails $ cabal' "show-build-info" ["lib:C", "-v1"]
    assertOutputContains "Internal error in target matching." r

    r <- fails $ cabal' "show-build-info" ["benchmarks", "-v1"]
    assertOutputContains "Cannot show-build-info the benchmarks in the package A-0.1.0.0 because it does not contain any benchmarks." r

