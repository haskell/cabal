import Test.Cabal.Prelude

main = cabalTest $ do
    -- your test code here
    res <- cabal' "run" ["foo", "+RTS"]
    assertOutputContains       "Warning" res

    res <- cabal' "run" ["foo", "+RTS", "--"]
    assertOutputContains       "Warning" res

    res <- cabal' "run" ["foo", "--", "+RTS"]
    assertOutputDoesNotContain "Warning" res
