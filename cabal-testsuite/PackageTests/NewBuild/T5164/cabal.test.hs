import Test.Cabal.Prelude
main = cabalTest $ do
    r1 <- recordMode DoNotRecord $ cabal' "v2-build" ["all"]
    assertOutputContains "Example data file" r1
