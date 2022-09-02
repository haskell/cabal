import Test.Cabal.Prelude

-- Unbounded (top) base with internal dependency: warn but do not error.
main = cabalTest $
  cabal "check" []
