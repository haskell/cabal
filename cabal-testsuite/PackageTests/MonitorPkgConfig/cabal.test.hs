import System.Directory (copyFile, createDirectoryIfMissing)
import Test.Cabal.Prelude

main = do
  skipIfWindows "no pkg-config on Windows CI"
  cabalTest $ do
    env <- getTestEnv

    cabal' "v2-build" ["--dry-run", "p", "-v2"]
      >>= assertOutputContains "Querying pkg-config database..."

    cabal' "v2-build" ["--dry-run", "p", "-v2"]
      >>= assertOutputDoesNotContain "Querying pkg-config database..."

    -- Check that changing PKG_CONFIG_PATH invalidates the cache

    let pkgConfigPath = testWorkDir env </> "pkgconfig"
    liftIO $ createDirectoryIfMissing True pkgConfigPath

    withEnv [("PKG_CONFIG_PATH", Just pkgConfigPath)] $ do
      cabal' "v2-build" ["--dry-run", "p", "-v2"]
        >>= assertOutputContains "Querying pkg-config database..."

      cabal' "v2-build" ["--dry-run", "p", "-v2"]
        >>= assertOutputDoesNotContain "Querying pkg-config database..."

      -- Check that changing a file in PKG_CONFIG_PATH invalidates the cache

      liftIO $ copyFile (testCurrentDir env </> "test.pc") (pkgConfigPath </> "test.pc")

      cabal' "v2-build" ["--dry-run", "p", "-v2"]
        >>= assertOutputContains "Querying pkg-config database..."

      cabal' "v2-build" ["--dry-run", "p", "-v2"]
        >>= assertOutputDoesNotContain "Querying pkg-config database..."
