import Test.Cabal.Prelude
-- Test that `cabal haddock --disable-documention` works as expected and leads
-- to a warning if a local package makes an outer reference.
main = cabalTest . withRepo "repo" $ do
    r <- cabal' "haddock" ["--disable-documentation", "B"]
    assertOutputContains "Warning: B: could not find link destinations for" r
