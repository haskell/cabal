import Test.Cabal.Prelude
-- Test build-tool-depends isn't influenced by PATH
main = cabalTest $ do
    env <- getTestEnv
    addToPath (testTmpDir env </> "scripts/") $ cabal "v2-build" ["client"]
