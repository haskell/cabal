import Test.Cabal.Prelude

-- #12539: SetupHooks run in the current working directory of cabal-install,
-- not in the package directory, and find the files of the package through the
-- working directory they are given.
main = cabalTest $ do
  mpkgdb <- testPackageDbPath <$> getTestEnv
  case mpkgdb of
    Nothing -> skip "Cabal-hooks library unavailable."
    Just _pkgdb -> recordMode DoNotRecord $ do
      r <- cabal' "v2-build" ["pkg"]
      assertOutputContains "pkg SetupHooks: working directory OK" r
