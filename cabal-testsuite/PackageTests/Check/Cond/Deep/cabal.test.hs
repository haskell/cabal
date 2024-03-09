import Test.Cabal.Prelude

-- `main-is` in both branches is not missing (deep).
main = cabalTest $
  cabal "check" []
