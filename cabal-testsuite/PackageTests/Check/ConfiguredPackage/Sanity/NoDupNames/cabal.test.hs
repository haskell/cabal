import Test.Cabal.Prelude

-- Duplicate section names.
main = cabalTest $
  fails $ cabal "check" []
