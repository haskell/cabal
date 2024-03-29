import Test.Cabal.Prelude
main = cabalTest $ do
    workdir <- fmap testWorkDir getTestEnv
    let conf = workdir </> "cabal-config"
    cabalG ["--config-file", conf] "user-config" ["init"]
    shouldExist conf
    fails $ cabalG ["--config-file", workdir </> "cabal-config"] "user-config" ["init"]
    cabalG ["--config-file", conf] "user-config" ["-f", "init"]
    shouldExist conf
    let conf2 = workdir </> "cabal-config2"
    withEnv [("CABAL_CONFIG", Just conf2)] $ do
        cabal "user-config" ["init"]
        shouldExist conf2
    cabalG ["--config-file", conf] "user-config" ["update", "-f", "-a", "extra-prog-path: foo", "-a", "extra-prog-path: bar"]
    assertFileDoesContain conf "foo,bar"
    cabalG ["--config-file", conf] "user-config" ["update", "-f", "-a", "extra-prog-path: foo, bar"]
    assertFileDoesContain conf "foo,bar"

    -- regression test for #6268 (password-command parsing)
    cabalG ["--config-file", conf]
        "user-config" ["update", "-f", "-a", "password-command: sh -c \"echo secret\""]
    -- non-quoted tokens do get quoted when writing, but this is expected
    assertFileDoesContain conf "password-command: \"sh\" \"-c\" \"echo secret\""
