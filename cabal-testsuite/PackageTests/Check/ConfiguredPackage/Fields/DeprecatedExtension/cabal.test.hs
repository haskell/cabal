import Test.Cabal.Prelude

-- Deprecated extension.
main = cabalTest $
  cabal "check" []
