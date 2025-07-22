import Test.Cabal.Prelude

-- `main-is` in both branches is not missing.
main = cabalTest $
  cabal "check" []
