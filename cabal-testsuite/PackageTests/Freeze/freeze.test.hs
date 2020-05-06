import Test.Cabal.Prelude
main = cabalTest $ do
    withRepo "repo" . withSourceCopy $ do
        cabal "v1-freeze" []
        cwd <- fmap testCurrentDir getTestEnv
        assertFileDoesNotContain (cwd </> "cabal.config") "exceptions"
        assertFileDoesNotContain (cwd </> "cabal.config") "my"
        assertFileDoesContain (cwd </> "cabal.config") "base"
