import Test.Cabal.Prelude
main = cabalTest $
  -- this should not fail, just warn.
   cabal "v2-build" ["all"]
