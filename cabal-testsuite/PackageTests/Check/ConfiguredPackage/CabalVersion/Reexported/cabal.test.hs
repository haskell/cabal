import Test.Cabal.Prelude

-- `reexported-module` need ≥1.22.
main = cabalTest $
  fails $ cabal "check" []
