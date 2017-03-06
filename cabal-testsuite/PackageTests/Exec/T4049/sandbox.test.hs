import Test.Cabal.Prelude
main = cabalTest $ do
    withSandbox $ do
        cabal "install" ["--enable-shared"]
        env <- getTestEnv
        let sandbox_dir = testSandboxDir env
            work_dir    = testWorkDir env
        gcc ["UseLib.c"
            , "-o", work_dir </> "UseLib"
            , "-l", "myforeignlib"
            , "-L", sandbox_dir </> "lib"]
        recordMode RecordAll $
            cabal "exec" ["-v0", work_dir </> "UseLib"]
