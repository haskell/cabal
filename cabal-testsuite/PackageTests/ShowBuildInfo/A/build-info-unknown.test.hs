import           Test.Cabal.Prelude

main = cabalTest $ do
    r <- fails $ cabal' "show-build-info" ["exe:B"]
    assertOutputContains "Internal error in target matching." r

    r <- fails $ cabal' "show-build-info" ["--unit-ids-json=B-inplace-0.1.0.0"]
    assertOutputContains "No unit B-inplace-0.1.0.0" r

    r <- fails $ cabal' "show-build-info" ["--unit-ids-json=A-0.1.0.0-inplace B-inplace-0.1.0.0"]
    assertOutputContains "No unit B-inplace-0.1.0.0" r

    r <- fails $ cabal' "show-build-info" ["--unit-ids-json=A-0.1.0.0-inplace", "exe:B"]
    assertOutputContains "Internal error in target matching." r
