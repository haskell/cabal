import Test.Cabal.Prelude

-- Deprecated extension.
main = cabalTest $
  fails $ cabal "check" []
