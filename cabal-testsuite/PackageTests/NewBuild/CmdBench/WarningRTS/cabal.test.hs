import Test.Cabal.Prelude

main = cabalTest $ do
    res <- cabal' "bench" ["foo", "+RTS"]
    assertOutputContains "Your RTS options are applied to cabal, not the executable. Use '--benchmark-options' " res

    res <- cabal' "bench" ["foo", "+RTS", "--"]
    assertOutputContains "Your RTS options are applied to cabal, not the executable. Use '--benchmark-options' " res

    res <- cabal' "bench" ["foo", "--benchmark-options=\"+RTS\""]
    assertOutputDoesNotContain "Warning: Your RTS options" res
