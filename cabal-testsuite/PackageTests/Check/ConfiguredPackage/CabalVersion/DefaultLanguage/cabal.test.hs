import Test.Cabal.Prelude

-- `default-language` need ≥1.10.
main = cabalTest $
  cabal "check" []
