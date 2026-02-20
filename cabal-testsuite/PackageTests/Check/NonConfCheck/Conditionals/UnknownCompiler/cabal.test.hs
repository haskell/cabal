import Test.Cabal.Prelude

-- Unknown compiler.
main = cabalTest $
  fails $ cabal "check" []
