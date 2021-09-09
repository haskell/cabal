import Test.Cabal.Prelude
main = cabalTest $ do
    r <- cabal' "man" ["--raw"]
    assertOutputContains ".B cabal install" r
    assertOutputDoesNotContain ".B cabal manpage" r
