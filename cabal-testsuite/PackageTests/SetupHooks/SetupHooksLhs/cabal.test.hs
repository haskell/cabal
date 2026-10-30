import Test.Cabal.Prelude

-- SetupHooks.lhs (literate Haskell) is accepted in place of SetupHooks.hs
-- (#12473). This needs to go through a build: `cabal check` already accepted
-- SetupHooks.lhs before.
main = cabalTest $ do
  mpkgdb <- testPackageDbPath <$> getTestEnv
  case mpkgdb of
    Nothing -> skip "Cabal-hooks library unavailable."
    Just _pkgdb -> recordMode DoNotRecord $
      cabal "v2-build" [ "all" ]
