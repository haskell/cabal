import Test.Cabal.Prelude

-- NONE license.
main = cabalTest $
  fails $ cabal "check" []
