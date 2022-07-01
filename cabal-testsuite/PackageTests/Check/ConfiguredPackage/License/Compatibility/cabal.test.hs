import Test.Cabal.Prelude

-- Compatibility w/ ≤1.4.
main = cabalTest $
  fails $ cabal "check" []
