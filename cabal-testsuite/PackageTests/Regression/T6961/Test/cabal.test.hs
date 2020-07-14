import Test.Cabal.Prelude
main = cabalTest $
  -- fails, but it shouldn't.
  -- https://github.com/haskell/cabal/issues/6961
  fails $ cabal "v2-build" ["all", "--dry-run", "--enable-tests"]
