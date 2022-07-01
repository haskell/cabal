import Test.Cabal.Prelude

-- No category.
main = cabalTest $
  fails $ cabal "check" []
