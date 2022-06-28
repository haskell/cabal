import Test.Cabal.Prelude

-- No version.
main = cabalTest $
  fails $ cabal "check" []
