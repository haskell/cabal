import Test.Cabal.Prelude

import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))

-- Test that `cabal install <exe>` installs only the named executable,
-- not every executable in the package. See #8614.
main = cabalTest $ do
  env <- getTestEnv
  recordMode DoNotRecord $ do
    let installdir = testPrefixDir env </> "bin"
        exeExt = if isWindows then "exe" else ""

    cabal "install"
      ["example1", "--installdir", installdir, "--overwrite-policy=always"]

    example1Installed <- liftIO $ doesFileExist (installdir </> "example1" <.> exeExt)
    example2Installed <- liftIO $ doesFileExist (installdir </> "example2" <.> exeExt)

    assertBool "example1 should have been installed" example1Installed
    assertBool "example2 should not have been installed" (not example2Installed)
