import Test.Cabal.Prelude
main = cabalTest $ do
    withRepo "repo" $ do
        cabal "v1-freeze" ["--disable-tests"]
        cwd <- fmap testCurrentDir getTestEnv
        assertFileDoesNotContain (cwd </> "cabal.config") "test-framework"
