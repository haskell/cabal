import Test.Cabal.Prelude
-- Test building a profiled library/executable which uses QuasiQuotes
-- (setup has to build the non-profiled version first)
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $ do
    skipUnless "no profiling libs" =<< hasProfiledLibraries
    setup_build ["--enable-library-profiling",
                 "--enable-profiling"]
