import Test.Cabal.Prelude

main = cabalTest $ do
    -- your test code here
    res <- cabal' "run" ["+RTS"]
    assertOutputContains       "Warning" res

    res <- cabal' "run" [ "+RTS", "--"]
    assertOutputContains       "Warning" res

    res <- cabal' "run" [ "--", "+RTS"]
    assertOutputDoesNotContain "Warning" res