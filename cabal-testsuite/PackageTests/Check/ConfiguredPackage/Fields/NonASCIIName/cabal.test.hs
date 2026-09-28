import Test.Cabal.Prelude

-- Non ASCII charaters in package name.
main = cabalTest $
  fails $ cabal "check" []
