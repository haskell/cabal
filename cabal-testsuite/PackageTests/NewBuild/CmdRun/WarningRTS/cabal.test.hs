import Test.Cabal.Prelude

main = cabalTest $ do
    res <- cabal' "run" ["foo", "+RTS"]
    assertOutputContains "Warning: Your RTS options" res

    res <- cabal' "run" ["foo", "+RTS", "--"]
    assertOutputContains "Warning: Your RTS options" res

    res <- cabal' "run" ["foo", "--", "+RTS"]
    assertOutputDoesNotContain "Warning: Your RTS options" res

    -- Regression test for https://github.com/haskell/cabal/issues/10487:
    -- 'cabal run -- +RTS' should not fail with "Unrecognised target '+RTS'"
    resNoTarget <- cabal' "run" ["--", "+RTS"]
    assertOutputDoesNotContain "Warning: Your RTS options" resNoTarget
    assertOutputDoesNotContain "Unrecognised target" resNoTarget
