import           Test.Cabal.Prelude

main = cabalTest $ do
    -- Make sure the vendored dependency D1 gets built
    cabal' "clean" []
    r <- cabal' "show-build-info" ["-v1", "D", "D1"]
    assertOutputContains "Building library for D1-0.1.0.0.." r
    assertOutputDoesNotContain "Building library for D-0.1.0.0.." r
