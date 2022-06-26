import Test.Cabal.Prelude

-- test-suite need ≥1.8.
main = cabalTest $
  fails $ cabal "check" []
