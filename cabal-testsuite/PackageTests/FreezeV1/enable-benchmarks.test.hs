import Test.Cabal.Prelude
main = cabalTest $ do
    withRepo "repo" $ do
        cabal "v1-freeze" ["--enable-benchmarks"]
        cwd <- fmap testCurrentDir getTestEnv
        assertFileDoesContain (cwd </> "cabal.config") "criterion"
        assertFileDoesContain (cwd </> "cabal.config") "ghc-prim"
