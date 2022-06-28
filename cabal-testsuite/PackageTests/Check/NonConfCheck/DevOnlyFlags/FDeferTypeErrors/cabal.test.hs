import Test.Cabal.Prelude

-- -fdefer-type-errors
main = cabalTest $
  fails $ cabal "check" []
