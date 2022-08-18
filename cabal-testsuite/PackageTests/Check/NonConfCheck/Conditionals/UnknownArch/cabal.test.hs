import Test.Cabal.Prelude

-- Uknown arch name.
main = cabalTest $
  fails $ cabal "check" []
