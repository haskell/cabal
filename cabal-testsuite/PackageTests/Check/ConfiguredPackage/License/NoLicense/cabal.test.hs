import Test.Cabal.Prelude

-- No license.
main = cabalTest $
  fails $ cabal "check" []
