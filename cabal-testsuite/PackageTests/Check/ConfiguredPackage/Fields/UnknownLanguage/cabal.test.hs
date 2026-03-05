import Test.Cabal.Prelude

-- Unknown language.
main = cabalTest $
  fails $ cabal "check" []
