import Test.Cabal.Prelude
main = cabalTest $
  -- this should fail,
  -- none of GHC have extension declared in other-extensions
  fails $ cabal "v2-build" ["all"]
