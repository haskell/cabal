import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    skipIfNoProfiledLibraries
    setup "configure" ["--enable-profiling"]
    setup "build" []
