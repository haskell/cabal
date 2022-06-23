import Test.Cabal.Prelude

-- Invalid `tested-with` range.
main = cabalTest $
  fails $ cabal "check" []
