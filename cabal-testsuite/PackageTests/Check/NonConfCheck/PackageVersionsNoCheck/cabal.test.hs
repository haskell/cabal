import Test.Cabal.Prelude

-- Internal targets (tests, benchmarks) should not be checked.
main = cabalTest $
  cabal "check" []
