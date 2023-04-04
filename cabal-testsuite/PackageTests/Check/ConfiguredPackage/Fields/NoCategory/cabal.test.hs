import Test.Cabal.Prelude

-- No category.
main = cabalTest $
  cabal "check" []
