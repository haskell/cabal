import Test.Cabal.Prelude

-- A user-specified pkg-config location must be the pkg-config the solver
-- reads its database through, not only the one Setup configure runs: the
-- fake pkg-config here is the only one that knows 'fakelib', so the build
-- can only be planned if the solver asked it.
main = cabalTest . recordMode DoNotRecord $ do
  liftIO $ skipIfWindows "the fake pkg-config is a shell script"
  cdir <- testCurrentDir <$> getTestEnv
  r <- cabal' "v2-build" ["--with-pkg-config=" ++ (cdir </> "pkg-config"), "all"]
  assertOutputContains "Dependency fakelib: using version 1.0" r
