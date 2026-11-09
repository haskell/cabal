import Test.Cabal.Prelude
main = cabalTest $ do
  mpkgdb <- testPackageDbPath <$> getTestEnv
  case mpkgdb of
    Nothing -> skip "Cabal-hooks library unavailable."
    Just _pkgdb -> do
      cabal "run" []
      withDirectory "inner" $
        cabal "run" ["exe:NonHs"]
