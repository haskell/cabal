import Test.Cabal.Prelude

-- Internal libraries missing upper bound are correctly reported.
main = cabalTest $
  cabal "check" []
