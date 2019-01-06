import Test.Cabal.Prelude
main = cabalTest $ do
    r <- fails $ cabal' "v2-build" []
    assertOutputContains "cycl" r -- match cyclic or cycle
    assertOutputContains "bar" r
    assertOutputContains "foo" r
    assertOutputContains "DepCycle" r
