import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    skipUnless "no profiling libs" =<< hasProfiledLibraries
    setup "configure" ["--enable-profiling"]
    setup "build" []
