import Test.Cabal.Prelude

main = do
  cabalTest $ do
    res <- cabal' "run" ["foo", "+RTS"]
    assertOutputContains "Warning: Your RTS options" res

    res <- cabal' "run" ["foo", "+RTS", "--"]
    assertOutputContains "Warning: Your RTS options" res

    res <- cabal' "run" ["foo", "--", "+RTS"]
    assertOutputDoesNotContain "Warning: Your RTS options" res

  -- Regression tests for https://github.com/haskell/cabal/issues/10487:
  -- 'cabal run -- +RTS' should not fail with "Unrecognised target '+RTS'"
  cabalTest' "no-target" $ do
    res <- cabal' "run" ["+RTS"]
    assertOutputContains "Warning: Your RTS options" res

    res <- cabal' "run" ["+RTS", "--"]
    assertOutputContains "Warning: Your RTS options" res

    res <- cabal' "run" ["--", "+RTS"]
    assertOutputDoesNotContain "Warning: Your RTS options" res
    assertOutputDoesNotContain "Unrecognised target" res
