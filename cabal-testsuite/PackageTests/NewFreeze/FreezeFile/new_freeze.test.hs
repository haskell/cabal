import Test.Cabal.Prelude
import Control.Monad.IO.Class
import Data.Char
import System.Directory

-- Test for 'cabal v2-freeze' with only a single library dependency.
-- my-local-package depends on my-library-dep, which has versions 1.0 and 2.0.
main = withShorterPathForNewBuildStore $ \storeDir ->
  cabalTest $
    withRepo "repo" $ do
      cwd <- fmap testCurrentDir getTestEnv
      let freezeFile = cwd </> "cabal.project.freeze"

      shouldNotExist freezeFile

      -- v2-build should choose the latest version for the dependency.
      cabalG' ["--store-dir=" ++ storeDir] "v2-build" ["--dry-run"] >>= assertUsesLatestDependency

      -- should not create freeze file with --dry-run or --only-download flags
      cabalG' ["--store-dir=" ++ storeDir] "v2-freeze" ["--dry-run"]
      cabalG' ["--store-dir=" ++ storeDir] "v2-freeze" ["--only-download"]
      shouldNotExist freezeFile

      -- Freeze a dependency on the older version.
      cabalG ["--store-dir=" ++ storeDir] "v2-freeze" ["--constraint=my-library-dep==1.0"]

      -- The file should constrain the dependency, but not the local package.
      shouldExist freezeFile
      assertFileDoesContain freezeFile "any.my-library-dep ==1.0"
      assertFileDoesNotContain freezeFile "my-local-package"

      -- cabal should be able to build the package using the constraint from the
      -- freeze file.
      cabalG' ["--store-dir=" ++ storeDir] "v2-build" [] >>= assertDoesNotUseLatestDependency

      -- Re-running v2-freeze should not change the constraints, because cabal
      -- should use the existing freeze file when choosing the new install plan.
      cabalG ["--store-dir=" ++ storeDir] "v2-freeze" []
      assertFileDoesContain freezeFile "any.my-library-dep ==1.0"

      -- cabal should choose the latest version again after the freeze file is
      -- removed.
      liftIO $ removeFile freezeFile
      cabalG' ["--store-dir=" ++ storeDir] "v2-build" ["--dry-run"] >>= assertUsesLatestDependency

      -- Re-running v2-freeze with no constraints or freeze file should constrain
      -- the dependency to the latest version.
      cabalG ["--store-dir=" ++ storeDir] "v2-freeze" []
      assertFileDoesContain freezeFile "any.my-library-dep ==2.0"
      assertFileDoesNotContain freezeFile "my-local-package"
    where
      assertUsesLatestDependency out = do
        assertOutputContains "my-library-dep-2.0 (lib)" out
        assertOutputDoesNotContain "my-library-dep-1.0" out

      assertDoesNotUseLatestDependency out = do
        assertOutputContains "my-library-dep-1.0 (lib)" out
        assertOutputDoesNotContain "my-library-dep-2.0" out
