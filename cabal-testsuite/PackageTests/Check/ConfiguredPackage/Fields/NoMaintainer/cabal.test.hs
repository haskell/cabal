import Test.Cabal.Prelude

-- No maintainer.
main = cabalTest $
  fails $ cabal "check" []
