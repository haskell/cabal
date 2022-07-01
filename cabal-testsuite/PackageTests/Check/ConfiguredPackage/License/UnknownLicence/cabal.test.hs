import Test.Cabal.Prelude

-- Unknown license.
main = cabalTest $
  fails $ cabal "check" []
