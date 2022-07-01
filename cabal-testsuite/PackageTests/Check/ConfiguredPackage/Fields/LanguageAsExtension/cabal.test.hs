import Test.Cabal.Prelude

-- Language listed as extension.
main = cabalTest $
  fails $ cabal "check" []
