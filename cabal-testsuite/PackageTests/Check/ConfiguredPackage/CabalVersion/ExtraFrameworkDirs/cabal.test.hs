import Test.Cabal.Prelude

-- `extra-framework-dirs` need ≥1.24. (just warning)
main = cabalTest $
  cabal "check" []
