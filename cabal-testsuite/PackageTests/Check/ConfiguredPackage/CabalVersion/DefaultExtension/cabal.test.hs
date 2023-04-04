import Test.Cabal.Prelude

-- `default-extensions` need ≥1.10.
main = cabalTest $
  cabal "check" []
