import Test.Cabal.Prelude
-- Test that `cabal haddock --haddock-all` correctly finds the .haddock
-- interface file for an internal sub-library (no "could not find link
-- destinations" warning).
main = cabalTest $ do
    res <- cabal' "haddock" ["--haddock-all"]
    assertOutputDoesNotContain "Could not find documentation for exported module" res
