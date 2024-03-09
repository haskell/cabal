import Test.Cabal.Prelude

-- `main-is` in both branches is not missing (after).
main = cabalTest $
  cabal "check" []
