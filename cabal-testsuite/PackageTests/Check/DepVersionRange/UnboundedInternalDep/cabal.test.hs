import Test.Cabal.Prelude

-- Unbounded with internal dependency: do not warn.
main = cabalTest $
  cabal "check" []
