import Test.Cabal.Prelude

-- Unused flag.
main = cabalTest $
  cabal "check" []
