import Test.Cabal.Prelude

main = cabalTest $ do
    res <- cabal' "test" ["foo", "+RTS"]
    assertOutputContains "Your RTS options are applied to cabal, not the executable. Use '--test-options'" res

    res <- cabal' "test" ["foo", "+RTS", "--"]
    assertOutputContains "Your RTS options are applied to cabal, not the executable. Use '--test-options'" res

    res <- cabal' "test" ["foo", "--test-options=\"+RTS\""]
    assertOutputDoesNotContain "Your RTS options" res
