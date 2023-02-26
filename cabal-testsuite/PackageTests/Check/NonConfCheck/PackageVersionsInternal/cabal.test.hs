import Test.Cabal.Prelude

-- Unbounded (top) base with internal dependency: no warn, no error.
main = cabalTest $
  cabal "check" []
