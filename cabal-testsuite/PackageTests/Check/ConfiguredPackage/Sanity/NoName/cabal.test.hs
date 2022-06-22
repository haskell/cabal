import Test.Cabal.Prelude

-- No package name.
main = cabalTest $
  fails $ cabal "check" []
