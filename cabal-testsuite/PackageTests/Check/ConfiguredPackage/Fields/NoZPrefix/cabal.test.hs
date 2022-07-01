import Test.Cabal.Prelude

-- no z-prefixed package names
main = cabalTest $
  fails $ cabal "check" []
