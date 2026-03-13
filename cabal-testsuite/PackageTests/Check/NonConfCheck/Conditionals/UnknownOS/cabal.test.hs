import Test.Cabal.Prelude

-- Unknown OS name.
main = cabalTest $
  fails $ cabal "check" []
