import Test.Cabal.Prelude

-- Unknown compiler in `tested-with`.
main = cabalTest $
  fails $ cabal "check" []
