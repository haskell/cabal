{-# LANGUAGE CPP #-}
-- | Groups black-box tests of cabal-install and configures them to test
-- the correct binary.
--
-- This file should do nothing but import tests from other modules and run
-- them with the path to the correct cabal-install binary.
module Main
       where

-- Modules from Cabal.
import Distribution.Compat.CreatePipe (createPipe)
import Distribution.Compat.Environment (setEnv, getEnvironment)
import Distribution.Compat.Internal.TempFile (createTempDirectory)
import Distribution.Simple.Configure (findDistPrefOrDefault)
import Distribution.Simple.Program.Builtin (ghcPkgProgram, gccProgram, ghcProgram)
import Distribution.Simple.Program.Db
        (defaultProgramDb, requireProgram, setProgramSearchPath
        ,lookupProgramVersion)
import Distribution.Simple.Program.Find
        (ProgramSearchPathEntry(ProgramSearchPathDir), defaultProgramSearchPath)
import Distribution.Simple.Program.Types
        ( Program(..), simpleProgram, programPath)
import Distribution.Simple.Setup ( Flag(..) )
import Distribution.Simple.Utils ( findProgramVersion, copyDirectoryRecursive
                                 , installOrdinaryFile )
import Distribution.Verbosity (normal)
import Distribution.Version (anyVersion)

-- Third party modules.
import Control.Concurrent.Async (withAsync, wait)
import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import System.Directory
        ( canonicalizePath
        , findExecutable
        , getDirectoryContents
        , getTemporaryDirectory
        , doesDirectoryExist
        , removeDirectoryRecursive
        , doesFileExist )
import System.FilePath
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, Assertion, assertFailure)
import Control.Monad ( filterM, forM, unless, when )
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.IORef (newIORef, writeIORef, readIORef)
import System.Exit (ExitCode(..))
import System.IO (withBinaryFile, IOMode(ReadMode))
import System.Process (runProcess, waitForProcess)
import Text.Regex.Posix ((=~))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Data.ByteString (ByteString)

#if MIN_VERSION_base(4,6,0)
import System.Environment ( getExecutablePath )
#endif

-- | Test case.
data TestCase = TestCase
    { tcName :: String -- ^ Name of the shell script
    , tcBaseDirectory :: FilePath -- ^ The base directory where tests are found
                                  --   e.g., cabal-install/tests/IntegrationTests
    , tcCategory :: String -- ^ Category of test; e.g., custom, exec, freeze, ...
    , tcStdOutPath :: Maybe FilePath -- ^ File path of expected standard output
    , tcStdErrPath :: Maybe FilePath -- ^ File path of expected standard error
    }

-- | Test result.
data TestResult = TestResult
    { trExitCode :: ExitCode -- ^ Exit code of test script
    , trStdOut :: ByteString -- ^ Actual standard output
    , trStdErr :: ByteString -- ^ Actual standard error
    , trWorkingDirectory :: FilePath -- ^ Temporary working directory test was
                                     --   executed in.
    }

-- | Cabal executable
cabalProgram :: Program
cabalProgram = (simpleProgram "cabal") {
    programFindVersion = findProgramVersion "--numeric-version" id
  }

-- | Convert test result to user-friendly string message.
testResultToString :: TestResult -> String
testResultToString testResult =
    exitStatus ++ "\n" ++ workingDirectory ++ "\n\n" ++ stdOut ++ "\n\n" ++ stdErr
  where
    exitStatus = "Exit status: " ++ show (trExitCode testResult)
    workingDirectory = "Working directory: " ++ (trWorkingDirectory testResult)
    stdOut = "<stdout> was:\n" ++ C8.unpack (trStdOut testResult)
    stdErr = "<stderr> was:\n" ++ C8.unpack (trStdErr testResult)

-- | Returns the command that was issued, the return code, and the output text
run :: FilePath -> String -> [String] -> IO TestResult
run cwd path args = do
  -- path is relative to the current directory; canonicalizePath makes it
  -- absolute, so that runProcess will find it even when changing directory.
  path' <- canonicalizePath path

  (pid, hReadStdOut, hReadStdErr) <- do
    -- Create pipes for StdOut and StdErr
    (hReadStdOut, hWriteStdOut) <- createPipe
    (hReadStdErr, hWriteStdErr) <- createPipe
    -- Run the process
    env0 <- getEnvironment
    -- CABAL_BUILDDIR can interfere with test running, so
    -- be sure to clear it out.
    let env = filter ((/= "CABAL_BUILDDIR") . fst) env0
    pid <- runProcess path' args (Just cwd) (Just env)
      Nothing (Just hWriteStdOut) (Just hWriteStdErr)
    -- Return the pid and read ends of the pipes
    return (pid, hReadStdOut, hReadStdErr)
  -- Read subprocess output using asynchronous threads; we need to
  -- do this aynchronously to avoid deadlocks due to buffers filling
  -- up.
  withAsync (B.hGetContents hReadStdOut) $ \stdOutAsync -> do
    withAsync (B.hGetContents hReadStdErr) $ \stdErrAsync -> do
      -- Wait for the subprocess to terminate
      exitcode <- waitForProcess pid
      -- We can now be sure that no further output is going to arrive,
      -- so we wait for the results of the asynchronous reads.
      stdOut <- wait stdOutAsync
      stdErr <- wait stdErrAsync
      -- Done
      return $ TestResult exitcode stdOut stdErr cwd

-- | Get a list of all names in a directory, excluding all hidden or
-- system files/directories such as '.', '..'  or any files/directories
-- starting with a '.'.
listDirectory :: FilePath -> IO [String]
listDirectory directory = do
  fmap (filter notHidden) $ getDirectoryContents directory
  where
    notHidden = not . isHidden
    isHidden name = "." `isPrefixOf` name

-- | List a directory as per 'listDirectory', but return an empty list
-- in case the directory does not exist.
listDirectoryLax :: FilePath -> IO [String]
listDirectoryLax directory = do
  d <- doesDirectoryExist directory
  if d then
    listDirectory directory
  else
    return [ ]

-- | @pathIfExists p == return (Just p)@ if @p@ exists, and
-- @return Nothing@ otherwise.
pathIfExists :: FilePath -> IO (Maybe FilePath)
pathIfExists p = do
  e <- doesFileExist p
  if e then
    return $ Just p
    else
      return Nothing

-- | Checks if file @p@ matches a 'ByteString' @s@, subject
-- to line-break normalization.  Lines in the file which are
-- prefixed with @RE:@ are treated specially as a regular expression.
fileMatchesString :: FilePath -> ByteString -> IO Bool
fileMatchesString p s = do
  withBinaryFile p ReadMode $ \h -> do
    expected <- (C8.lines . normalizeLinebreaks) `fmap` B.hGetContents h -- Strict
    let actual = C8.lines $ normalizeLinebreaks s
    return $ length expected == length actual &&
             and (zipWith matches expected actual)
  where
    matches :: ByteString -> ByteString -> Bool
    matches pattern line
        | C8.pack "RE:" `B.isPrefixOf` pattern = line =~ C8.drop 3 pattern
        | otherwise                            = line == pattern

    -- This is a bit of a hack, but since we're comparing
    -- *text* output, we should be OK.
    normalizeLinebreaks = B.filter (not . ((==) 13)) -- carriage return

-- | Check if the @actual@ 'ByteString' matches the contents of
-- @expected@ (always succeeds if the expectation is 'Nothing').
-- Also takes the 'TestResult' and a label @handleName@ describing
-- what text the string values correspond to.
mustMatch :: TestResult -> String -> ByteString -> Maybe FilePath -> Assertion
mustMatch _          _          _      Nothing         =  return ()
mustMatch testResult handleName actual (Just expected) = do
  m <- fileMatchesString expected actual
  unless m $ assertFailure $
      "<" ++ handleName ++ "> did not match file '"
      ++ expected ++ "'.\n" ++ testResultToString testResult

-- | Given a @directory@, return its subdirectories.  This is
-- run on @cabal-install/tests/IntegrationTests@ to get the
-- list of test categories which can be run.
discoverTestCategories :: FilePath -> IO [String]
discoverTestCategories directory = do
  names <- listDirectory directory
  fmap sort $ filterM (\name -> doesDirectoryExist $ directory </> name) names

-- | Find all shell scripts in @baseDirectory </> category@.
discoverTestCases :: FilePath -> String -> IO [TestCase]
discoverTestCases baseDirectory category = do
  -- Find the names of the shell scripts
  names <- fmap (filter isTestCase) $ listDirectoryLax directory
  -- Fill in TestCase for each script
  forM (sort names) $ \name -> do
    stdOutPath <- pathIfExists $ directory </> name `replaceExtension` ".out"
    stdErrPath <- pathIfExists $ directory </> name `replaceExtension` ".err"
    return $ TestCase { tcName = name
                      , tcBaseDirectory = baseDirectory
                      , tcCategory = category
                      , tcStdOutPath = stdOutPath
                      , tcStdErrPath = stdErrPath
                      }
  where
    directory = baseDirectory </> category
    isTestCase name = ".sh" `isSuffixOf` name

-- | Given a list of 'TestCase's (describing a shell script for a
-- single test case), run @mk@ on each of them to form a runnable
-- 'TestTree'.
createTestCases :: [TestCase] -> (TestCase -> Assertion) -> IO [TestTree]
createTestCases testCases mk =
  return $ (flip map) testCases $ \tc -> testCase (tcName tc ++ suffix tc) $ mk tc
  where
    suffix tc = case (tcStdOutPath tc, tcStdErrPath tc) of
      (Nothing, Nothing) -> " (ignoring stdout+stderr)"
      (Just _ , Nothing) -> " (ignoring stderr)"
      (Nothing, Just _ ) -> " (ignoring stdout)"
      (Just _ , Just _ ) -> ""

-- | Given a 'TestCase', run it.
--
-- A test case of the form @category/testname.sh@ is
-- run in the following way
--
--      1. We make a full copy all of @category@ into a temporary
--         directory.
--      2. In the temporary directory, run @testname.sh@.
--      3. Test that the exit code is zero, and that stdout/stderr
--         match the expected results.
--
runTestCase :: TestCase -> IO ()
runTestCase tc = do
  doRemove <- newIORef False
  bracket createWorkDirectory (removeWorkDirectory doRemove) $ \workDirectory -> do
    -- Run
    let scriptDirectory = workDirectory
    sh <- fmap (fromMaybe $ error "Cannot find 'sh' executable") $
      findExecutable "sh"
    testResult <- run scriptDirectory sh [ "-e", tcName tc]
    -- Assert that we got what we expected
    case trExitCode testResult of
      ExitSuccess ->
        return () -- We're good
      ExitFailure _ ->
        assertFailure $ "Unexpected exit status.\n\n"
        ++ testResultToString testResult
    mustMatch testResult "stdout" (trStdOut testResult) (tcStdOutPath tc)
    mustMatch testResult "stderr" (trStdErr testResult) (tcStdErrPath tc)
    -- Only remove working directory if test succeeded
    writeIORef doRemove True
  where
    createWorkDirectory = do
      -- Create the temporary directory
      tempDirectory <- getTemporaryDirectory
      workDirectory <- createTempDirectory tempDirectory "cabal-install-test"
      -- Copy all the files from the category into the working directory.
      copyDirectoryRecursive normal
        (tcBaseDirectory tc </> tcCategory tc)
        workDirectory
      -- Copy in the common.sh stub
      let commonDest = workDirectory </> "common.sh"
      e <- doesFileExist commonDest
      unless e $
        installOrdinaryFile normal (tcBaseDirectory tc </> "common.sh") commonDest
      -- Done
      return workDirectory
    removeWorkDirectory doRemove workDirectory = do
        remove <- readIORef doRemove
        when remove $ removeDirectoryRecursive workDirectory

discoverCategoryTests :: FilePath -> String -> IO [TestTree]
discoverCategoryTests baseDirectory category = do
  testCases <- discoverTestCases baseDirectory category
  createTestCases testCases runTestCase

main :: IO ()
main = do
  -- Find executables and build directories, etc.
  distPref <- guessDistDir
  buildDir <- canonicalizePath (distPref </> "build/cabal")
  let programSearchPath = ProgramSearchPathDir buildDir : defaultProgramSearchPath
  (cabal, _) <- requireProgram normal cabalProgram
    (setProgramSearchPath programSearchPath defaultProgramDb)
  (ghcPkg, _) <- requireProgram normal ghcPkgProgram defaultProgramDb
  baseDirectory <- canonicalizePath $ "tests" </> "IntegrationTests"
  -- Set up environment variables for test scripts
  setEnv "GHC_PKG" $ programPath ghcPkg
  setEnv "CABAL" $ programPath cabal
  -- Define default arguments
  setEnv "CABAL_ARGS" $ "--config-file=config-file"
  setEnv "CABAL_ARGS_NO_CONFIG_FILE" " "
  -- Don't get Unicode output from GHC
  setEnv "LC_ALL" "C"
  -- Try to find something that can compile C programs
  findGcc <- lookupProgramVersion normal gccProgram anyVersion defaultProgramDb
  case findGcc of
    Left _ -> do
      -- set CC to GHC blabla
      (ghc, _) <- requireProgram normal ghcProgram defaultProgramDb
      setEnv "CCOMP" $ programPath ghc ++ " -no-hs-main"
    Right (gcc, _, _) -> do
      setEnv "CCOMP" (programPath gcc)
  -- Discover all the test categories
  categories <- discoverTestCategories baseDirectory
  -- Discover tests in each category
  tests <- forM categories $ \category -> do
    categoryTests <- discoverCategoryTests baseDirectory category
    return (category, categoryTests)
  -- Map into a test tree
  let testTree = map (\(category, categoryTests) ->
                        testGroup category categoryTests) tests
  -- Run the tests
  defaultMain $ testGroup "Integration Tests" $ testTree

-- See this function in Cabal's PackageTests. If you update this,
-- update its copy in cabal-install.  (Why a copy here? I wanted
-- to try moving this into the Cabal library, but to do this properly
-- I'd have to BC'ify getExecutablePath, and then it got hairy, so
-- I aborted and did something simple.)
guessDistDir :: IO FilePath
guessDistDir = do
#if MIN_VERSION_base(4,6,0)
    exe_path <- canonicalizePath =<< getExecutablePath
    let dist0 = dropFileName exe_path </> ".." </> ".."
    b <- doesFileExist (dist0 </> "setup-config")
#else
    let dist0 = error "no path"
        b = False
#endif
    -- Method (2)
    if b then canonicalizePath dist0
         else findDistPrefOrDefault NoFlag >>= canonicalizePath
