import Test.Cabal.Prelude
import System.FilePath ((</>))
import System.Directory (removeFile)

main = cabalTest $ do
  runTestForInstallMethod "symlink"
  runTestForInstallMethod "copy"

runTestForInstallMethod :: String -> TestM ()
runTestForInstallMethod method = do
  env <- getTestEnv
  let installdir = testPrefixDir env </> "bin"

  -- install the binary, don't overwrite anything
  cabal "install"
    ["p", "--installdir", installdir, "--install-method", method, "--overwrite-policy", "never"]
  -- install the binary, don't overwrite anything
  fails $ cabal "install"
    ["p", "--installdir", installdir, "--install-method", method, "--overwrite-policy", "never"]
  -- install the binary again, forcing an overwrite, should succeed.
  cabal "install"
    ["p", "--installdir", installdir, "--install-method", method, "--overwrite-policy", "always"]
  -- remove the installed binary.
  liftIO $ removeFile (installdir </> "p"  <.> exeExt)

  testPolicyForAffix installdir method ["--program-suffix", "-my-suffix"]
  testPolicyForAffix installdir method ["--program-prefix", "my-prefix-"]
  testPolicyForAffix installdir method ["--program-prefix", "my-prefix-", "--program-suffix", "-my-suffix"]
  -- remove the installed binaries.
  liftIO $ removeFile (installdir </> "p" <.> exeExt)
  liftIO $ removeFile (installdir </> "p-my-suffix" <.> exeExt)
  liftIO $ removeFile (installdir </> "my-prefix-p" <.> exeExt)
  liftIO $ removeFile (installdir </> "my-prefix-p-my-suffix" <.> exeExt)

-- | Run a policy test for a given 'install-method' and program-affix
-- (i.e., '--program-suffix' or '--program-prefix').
--
-- When a program affix is given, the installation should not be affected
-- by installing the binary with no affix and vice-versa.
-- So, installing the program without any affix is not affected by installations with
-- some program affix.
testPolicyForAffix :: FilePath -> String -> [String] -> TestM ()
testPolicyForAffix installdir method affixArgs = do
  -- install the binary again, forcing an overwrite, must succeed.
  -- The rest of this test assumes the binary has been installed before.
  cabal "install"
    ["p", "--installdir", installdir, "--install-method", method, "--overwrite-policy", "always"]

  -- Install the binary with some program affix, don't need overwrite anything
  cabal "install"
    (["p", "--installdir", installdir, "--install-method", method, "--overwrite-policy", "never"] ++ affixArgs)
  -- Once the binary is installed, we can't overwrite it unless we are told so.
  fails $ cabal "install"
    (["p", "--installdir", installdir, "--install-method", method, "--overwrite-policy", "never"] ++ affixArgs)
  -- Successfully overwrite the binary if told so.
  cabal "install"
    (["p", "--installdir", installdir, "--install-method", method, "--overwrite-policy", "always"] ++ affixArgs)

  -- remove the installed binary.
  liftIO $ removeFile (installdir </> "p" <.> exeExt)
  -- Make sure we can still install the original program with no program affix without overwriting,
  -- even though, the program is already installed with some affix.
  cabal "install"
    ["p", "--installdir", installdir, "--install-method", method, "--overwrite-policy", "never"]

exeExt = if isWindows then "exe" else ""
