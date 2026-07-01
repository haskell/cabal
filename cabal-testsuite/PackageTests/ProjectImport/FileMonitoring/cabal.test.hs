import Control.Monad.Trans.Reader
import Data.List
import Prelude hiding (log)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode(ExitFailure))
import System.IO
import Test.Cabal.OutputNormalizer
import Test.Cabal.Prelude

-- If tests are enabled then we get this output:
-- [1 of 1] Compiling Main
-- test/Main.hs:4:8: error: [GHC-88464]
--     Variable not in scope: puStrLn :: [Char] -> IO ()
--     Suggested fix: Perhaps use ‘putStrLn’ (imported from Prelude)
--   |
-- 4 | main = puStrLn "Test suite not yet implemented."
--   |
main = do
  cabalTest' "main-project" . recordMode RecordMarked $ do
    let opts = ["--project-file=cabal.project"]
    expectedMonitoring <-
      -- TODO: When fixed, use expected output, not the actual output and delete
      -- the actual output file.
      -- readFileVerbatim "cabal.main-project.expect.txt"
      readFileVerbatim "cabal.main-project.actual.txt"
    runProjectTest expectedMonitoring opts
    runCommandTest opts
    runConfigureTest "cabal.project.local" opts

  -- Don't run the configure test for the local-only project because the
  -- configure command will back up and rename the existing .local file.
  cabalTest' "local-only" . recordMode RecordMarked $ do
    let opts = ["--project-file=cabal.local-only.project"]
    expectedMonitoring <-
      -- TODO: When fixed, use expected output, not the actual output and delete
      -- the actual output file.
      -- readFileVerbatim "cabal.local-only.expect.txt"
      readFileVerbatim "cabal.local-only.actual.txt"
    runProjectTest expectedMonitoring opts
    runCommandTest opts

  cabalTest' "freeze-only" . recordMode RecordMarked $ do
    let opts = ["--project-file=cabal.freeze-only.project"]
    expectedMonitoring <-
      -- TODO: When fixed, use expected output, not the actual output and delete
      -- the actual output file.
      -- readFileVerbatim "cabal.freeze-only.expect.txt"
      readFileVerbatim "cabal.freeze-only.actual.txt"
    runProjectTest expectedMonitoring opts
    runCommandTest opts
    runConfigureTest "cabal.freeze-only.project.local" opts

testNotYetImplementedMsg :: String
testNotYetImplementedMsg = "Test suite not yet implemented"

failureMsg :: String
failureMsg = "Failed to build cabal-project-repro-0.1.0.0-inplace-cabal-project-repro-test."

log :: String -> ReaderT TestEnv IO ()
log = recordHeader . pure

-- | Run the build command, with tests disabled then enabled on the command
-- line.
runCommandTest :: [String] -> ReaderT TestEnv IO ()
runCommandTest projOpts = do
  log "Disabling tests on the command line"
  cmdDisabledTests <- cabal' "build" (projOpts ++ ["--disable-tests"])
  assertOutputDoesNotContain testNotYetImplementedMsg cmdDisabledTests

  log "Enabling tests on the command line"
  cmdEnabledTests <- fails $ cabal' "build" (projOpts ++ ["--enable-tests"])
  assertOutputContains testNotYetImplementedMsg cmdEnabledTests

-- | Run the build command, after configuring for tests to be disabled then for
-- tests to be enabled. The project .local is the place where this configuration
-- is saved. The .local file that the configure command creates is deleted at
-- the conclusion of this test.
runConfigureTest :: String -> [String] -> ReaderT TestEnv IO ()
runConfigureTest projectLocalFile projOpts = do
  cwd <- testCurrentDir <$> getTestEnv
  let localFile = cwd </> projectLocalFile
  haveLocal <- liftIO $ doesFileExist localFile
  let existsMsg = if haveLocal then "It exists." else "It was not found."
  log $ "Checking for the existence of: " ++ localFile ++ ". " ++ existsMsg
  when haveLocal . liftIO $ removeFile localFile

  log "Disabling tests with configure command"
  -- The `configure` command will create a .local file disabling tests.
  _ <- cabal' "configure" (projOpts ++ ["--disable-tests"])
  haveLocal <- liftIO $ doesFileExist localFile
  unless haveLocal $ assertFailure "Was not able to find .local project file after configure command"
  assertFileDoesContain localFile "tests: False"

  cmdDisabledTests <- cabal' "build" projOpts
  assertOutputDoesNotContain testNotYetImplementedMsg cmdDisabledTests
  -- Revert the change, delete the .local file `configure` adds.
  liftIO $ removeFile localFile

  log "Enabling tests on the command line"
  -- The `configure` command will create a .local file enabbling tests.
  _ <- cabal' "configure" (projOpts ++ ["--enable-tests"])
  haveLocal <- liftIO $ doesFileExist localFile
  unless haveLocal $ assertFailure "Was not able to find .local project file after configure command"
  assertFileDoesContain localFile "tests: True"

  cmdEnabledTests <- fails $ cabal' "build" (projOpts ++ ["--enable-tests"])
  assertOutputContains testNotYetImplementedMsg cmdEnabledTests
  -- Revert the change, delete the .local file `configure` adds.
  liftIO $ removeFile localFile

-- | Runs the build command that should fail but then a write to an imported
-- project configuration file should be noticed by file monitoring.
--
-- WARNING: Don't run other tests before `runProjectTest` otherwise the
-- expected monitoring output will have already been output and missed.
runProjectTest :: String -> [String] -> ReaderT TestEnv IO ()
runProjectTest expectMsg projOpts = do
  log "As-is, the project should not build"
  projEnabledTests <- fails $ cabal' "build" projOpts
  env <- getTestEnv
  norm_env <- mkNormalizerEnv
  let actual = normalizeOutput norm_env (resultOutput projEnabledTests)
  unless (expectMsg `isInfixOf` actual) $ assertFailure $ "Can't find expected output:\n" ++ expectMsg
  assertExitCode (ExitFailure 1) projEnabledTests
  assertOutputContains failureMsg projEnabledTests

  -- Change the imported project file with "tests: False".
  log "Rewriting an imported project file to disable tests"
  cwd <- testCurrentDir <$> getTestEnv
  let configFile = cwd </> "test" </> "tests-toggle.config"
  liftIO $ writeFile configFile "package *\n  tests: False"
  -- TODO: If project imports were properly monitored, the build
  -- should succeed without a clean. When fixed, remove the clean.
  log "A clean should not be necessary with proper monitoring of files a project imports."
  _ <- cabal' "clean" projOpts
  projDisabledTests <- cabal' "build" projOpts
  assertOutputDoesNotContain testNotYetImplementedMsg projDisabledTests
  assertOutputDoesNotContain failureMsg projDisabledTests
  -- Revert the change.
  liftIO $ writeFile configFile "package *\n  tests: True"
