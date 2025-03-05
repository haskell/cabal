import Test.Cabal.Prelude

main = cabalTest $ do
    r <- fails $ cabal' "check" ["--ignore=unknown-directory"]
    assertOutputContains "is invalid on Windows" r
    assertOutputContains "Aux" r
    assertOutputContains "Exe/Aux" r
    assertOutputContains "Test/Aux" r
    assertOutputContains "Bench/Aux" r
    assertOutputContains "Exe/Aux/Test" r
    assertOutputContains "Test/Aux/Test" r
    assertOutputContains "Bench/Aux/Test" r

