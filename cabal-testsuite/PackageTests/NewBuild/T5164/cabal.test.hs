import Test.Cabal.Prelude
main = cabalTest $ do
    r1 <- recordMode DoNotRecord $ cabal' "new-build" ["all"]
    assertOutputContains "Example data file" r1
