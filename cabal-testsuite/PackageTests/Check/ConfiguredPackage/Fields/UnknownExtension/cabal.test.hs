import Test.Cabal.Prelude

-- Unknown extension.
main = cabalTest $
  fails $ cabal "check" []
