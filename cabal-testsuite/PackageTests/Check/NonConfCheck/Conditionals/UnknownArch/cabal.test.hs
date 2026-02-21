import Test.Cabal.Prelude

-- Unknown arch name.
main = cabalTest $
  fails $ cabal "check" []
