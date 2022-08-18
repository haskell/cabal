import Test.Cabal.Prelude

-- No exec, library, test or benchmark.
main = cabalTest $
  fails $ cabal "check" []
