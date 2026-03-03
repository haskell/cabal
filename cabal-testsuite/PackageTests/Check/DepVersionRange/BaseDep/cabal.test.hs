import Test.Cabal.Prelude

-- Unbounded (top) base.
main = cabalTest $
  fails $ cabal "check" []
