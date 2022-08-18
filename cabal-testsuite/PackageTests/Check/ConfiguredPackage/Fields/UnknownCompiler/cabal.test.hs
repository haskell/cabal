import Test.Cabal.Prelude

-- Uknown compiler in `tested-with`.
main = cabalTest $
  fails $ cabal "check" []
