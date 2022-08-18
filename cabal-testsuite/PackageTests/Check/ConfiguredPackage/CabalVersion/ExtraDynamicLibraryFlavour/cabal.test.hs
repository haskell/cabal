import Test.Cabal.Prelude

-- `extra-dynamic-library-flavour` need ≥3.0.
main = cabalTest $
  fails $ cabal "check" []
