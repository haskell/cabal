import Test.Cabal.Prelude

-- Sublibraries missing upper bound are correctly reported.
main = cabalTest $
  cabal "check" []
