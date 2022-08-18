import Test.Cabal.Prelude

-- Uknown compiler.
main = cabalTest $
  fails $ cabal "check" []
