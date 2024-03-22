import Test.Cabal.Prelude
main = cabalTest $ do
    withRepo "repo" $ do
        cabal "v1-freeze" ["--enable-tests"]
        cwd <- fmap testCurrentDir getTestEnv
        assertFileDoesContain (cwd </> "cabal.config") "test-framework"
