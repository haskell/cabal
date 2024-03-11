import Test.Cabal.Prelude

-- `main-is` in both branches is not missing (deep, actually missing).
main = cabalTest $
  fails $ cabal "check" []
