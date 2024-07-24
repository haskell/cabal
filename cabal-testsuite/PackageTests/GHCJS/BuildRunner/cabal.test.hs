import Test.Cabal.Prelude

main = do
  cabalTest . expectBrokenIfWindows 10179 . recordMode DoNotRecord $ do
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
    assertOutputContains magicString output
  where
    fakeGhcjsPath = "scripts/fake-ghcjs.sh"
