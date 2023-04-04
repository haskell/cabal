import Test.Cabal.Prelude

-- No maintainer.
main = cabalTest $
  cabal "check" []
