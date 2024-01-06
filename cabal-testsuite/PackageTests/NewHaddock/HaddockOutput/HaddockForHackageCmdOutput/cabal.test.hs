import Test.Cabal.Prelude

-- Test that `cabal haddock --haddock-for-hackage` does not report that it
-- creates an `index.html` file.
main = cabalTest $ do
  r <- cabal' "haddock" ["--haddock-for-hackage", "A"]
  assertOutputDoesNotContain "index.html" r
