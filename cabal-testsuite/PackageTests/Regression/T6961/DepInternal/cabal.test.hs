import Test.Cabal.Prelude
main = cabalTest $
  -- we try to depend on private component from outside,
  -- so this should fail.
  fails $ cabal "v2-build" ["all", "--dry-run", "--enable-tests"]
