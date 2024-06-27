import Distribution.Compat.Environment (setEnv)
import System.Directory (copyFile, createDirectoryIfMissing, removeDirectoryRecursive)
import Test.Cabal.Prelude

main = cabalTest $ do
  env <- getTestEnv

  cabal' "v2-build" ["--dry-run", "p", "-v2"]
    >>= assertOutputContains "Querying pkg-config database..."

  cabal' "v2-build" ["--dry-run", "p", "-v2"]
    >>= assertOutputDoesNotContain "Querying pkg-config database..."

  -- Check that changing PKG_CONFIG_PATH invalidates the cache

  let pkgConfigPath = testWorkDir env </> "pkgconfig"
  liftIO $ do
    createDirectoryIfMissing True pkgConfigPath
    setEnv "PKG_CONFIG_PATH" pkgConfigPath

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
