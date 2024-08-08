import Test.Cabal.Prelude

main = do
  cabalTest . recordMode DoNotRecord $ do
    cwd <- fmap testCurrentDir getTestEnv
    testInvokedWithBuildRunner cwd "test" []
    testInvokedWithBuildRunner cwd "run" ["ghcjs-exe"]
    testInvokedWithBuildRunner cwd "bench" []

magicString = "SUCCESS! GHCJS was invoked with '-build-runner' option"

testInvokedWithBuildRunner cwd cabalCmd extraArgs = do
    output <- fails $ cabal' cabalCmd $ extraArgs ++
        [ "--ghcjs"
        , "--with-compiler", cwd </> fakeGhcjsPath
        ]
        -- On windows point cabal to the right cc
        ++ if isWindows then ["--with-gcc", "scripts/cc.bat"] else []
    assertOutputContains magicString output
  where
    fakeGhcjsPath = if isWindows then "scripts/fake-ghcjs.exe" else "scripts/fake-ghcjs.sh"
