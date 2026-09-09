import Test.Cabal.Prelude

-- Under cross-compilation the pkg-config database is read once per stage,
-- through that stage's toolchain, and a 'package host:*' or 'package build:*'
-- stanza can give each stage its own pkg-config. Each stage's fake pkg-config
-- here knows 'fakelib' at a different version: 'tool', a build-tool
-- dependency and so built for the build stage, must be configured against the
-- build stage's version and 'app' against the host's.
--
-- Requires --with-build-compiler to be passed to the test runner; skipped
-- otherwise.
main = cabalTest . recordMode DoNotRecord $ do
  liftIO $ skipIfWindows "the fake pkg-config is a shell script"
  withBuildCompiler $ \bc -> do
    cdir <- testCurrentDir <$> getTestEnv
    liftIO . writeFile (cdir </> "cabal.project.local") $
      unlines
        [ "package host:*"
        , "  pkg-config-location: " ++ (cdir </> "host" </> "pkg-config")
        , "package build:*"
        , "  pkg-config-location: " ++ (cdir </> "build" </> "pkg-config")
        ]
    r <- cabal' "v2-build" ["--with-build-compiler=" ++ bc, "all"]
    assertOutputContains "Dependency fakelib: using version 1.0" r
    assertOutputContains "Dependency fakelib: using version 2.0" r
