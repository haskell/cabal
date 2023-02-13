import Test.Cabal.Prelude

main = cabalTest $ do
    res <- cabal' "run" ["foo", "+RTS"]
    assertOutputContains "Warning: Your RTS options" res

    res <- cabal' "run" ["foo", "+RTS", "--"]
    assertOutputContains "Warning: Your RTS options" res

    res <- cabal' "run" ["foo", "--", "+RTS"]
    assertOutputDoesNotContain "Warning: Your RTS options" res
