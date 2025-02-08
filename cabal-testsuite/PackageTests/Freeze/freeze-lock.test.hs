import Test.Cabal.Prelude
main = cabalTest $ do
    withRepo "repo" $ do
        cabal "v2-freeze" ["--lock"]
        cwd <- fmap testCurrentDir getTestEnv
        assertFileDoesContain (cwd </> "cabal.project.freeze") "reject-unconstrained-dependencies: all"
