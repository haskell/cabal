import Test.Cabal.Prelude
-- Test if detailed-0.9 builds correctly and runs
-- when linked dynamically
-- See https://github.com/haskell/cabal/issues/4270
main = setupAndCabalTest $ do
  skipIfAllCabalVersion "< 2.2"
  skipIfNoSharedLibraries
  skipUnless "no shared Cabal"  =<< hasCabalShared
  expectBrokenIfOSXAndGhc "== 8.0.2" 8028 $ do
    setup_build ["--enable-tests", "--enable-executable-dynamic"]
    setup "test" []
