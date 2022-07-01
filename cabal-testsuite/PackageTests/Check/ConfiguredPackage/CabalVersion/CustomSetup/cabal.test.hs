import Test.Cabal.Prelude

-- `custom-setup` on ≥1.24.
main = cabalTest $
  cabal "check" []
