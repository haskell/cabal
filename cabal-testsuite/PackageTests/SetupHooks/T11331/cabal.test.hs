import Test.Cabal.Prelude
import System.Directory ( copyFile )

-- Test for https://github.com/haskell/cabal/issues/11331
main = cabalTest $ do

  mpkgdb <- testPackageDbPath <$> getTestEnv
  case mpkgdb of
    Nothing -> skip "Cabal-hooks library unavailable."
    Just _pkgdb -> recordMode DoNotRecord $ do

      cwd <- testCurrentDir <$> getTestEnv
      let
        cabalTemplate = cwd </> "T11331.cabal.in"
        cabalFile = cwd </> "T11331.cabal"

      -- Add a dependency on 'containers == 0.7' to the library stanza.
      liftIO $ do
        copyFile cabalTemplate cabalFile
        appendFile cabalFile "  build-depends: containers == 0.7"

      cabal "v2-build" []

      cabal "v2-clean" []

      -- Change the dependency to 'containers == 0.8'
      liftIO $ do
        copyFile cabalTemplate cabalFile
        appendFile cabalFile "  build-depends: containers == 0.8"

      cabal "v2-build" []
