import Test.Cabal.Prelude
-- Test --ignore-build-tools ignores build-tools and build-tool-depends
main = setupTest $ do
    setup "configure" ["--ignore-build-tools"]
    setup "build" []
