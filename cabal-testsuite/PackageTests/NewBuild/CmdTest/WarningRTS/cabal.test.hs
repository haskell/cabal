import Test.Cabal.Prelude

main = cabalTest $ do
    res <- cabal' "test" ["foo", "+RTS"]
    assertOutputContains "Some RTS options were found standalone" res

    res <- cabal' "test" ["foo", "--test-options=\"+RTS\"", "+RTS"]
    assertOutputContains "Some RTS options were found standalone" res

    res <- cabal' "test" ["foo", "--test-options=\"+RTS\""]
    assertOutputDoesNotContain "Some RTS options were found standalone" res
