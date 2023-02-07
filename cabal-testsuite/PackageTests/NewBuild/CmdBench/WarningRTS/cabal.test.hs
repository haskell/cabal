import Test.Cabal.Prelude

main = cabalTest $ do
    res <- cabal' "bench" ["foo", "+RTS"]
    assertOutputContains "Some RTS options were found standalone" res

    res <- cabal' "bench" ["foo", "--benchmark-options=\"+RTS\"", "+RTS"]
    assertOutputContains "Some RTS options were found standalone" res

    res <- cabal' "bench" ["foo", "--benchmark-options=\"+RTS\""]
    assertOutputDoesNotContain "Some RTS options were found standalone" res
