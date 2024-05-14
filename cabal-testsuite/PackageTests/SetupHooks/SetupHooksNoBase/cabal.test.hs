import Test.Cabal.Prelude

-- Test that we can compile the Setup.hs script for a package with
-- build-type:Hooks without requiring a dependency on 'base'.
--
-- NB: we specifically don't include a 'Setup.hs' file in this package,
-- as we rely on 'cabal-install' generating one that does not incur an extra
-- dependency on base.
main = cabalTest $ do
  mpkgdb <- testPackageDbPath <$> getTestEnv
  case mpkgdb of
    Nothing -> skip "Cabal-hooks library unavailable."
    Just _pkgdb -> recordMode DoNotRecord $ do
      cabal "v2-build" [ "all" ]
