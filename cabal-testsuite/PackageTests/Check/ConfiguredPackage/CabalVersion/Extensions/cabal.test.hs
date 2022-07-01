import Test.Cabal.Prelude

-- `extensions` deprecated.
main = cabalTest $
  fails $ cabal "check" []
