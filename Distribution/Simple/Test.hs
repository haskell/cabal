-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Test
-- Copyright   :  Thomas Tuegel 2010
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point into testing a built package. Performs the
-- \"@.\/setup test@\" action. It runs test suites designated in the package
-- description and reports on the results.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.Test
    ( test
    , runTests
    , writeSimpleTestStub
    , stubFilePath
    , stubName
    ) where

import Distribution.ModuleName ( ModuleName )
import Distribution.Package ( PackageIdentifier(..), PackageName(..) )
import Distribution.PackageDescription
    ( PackageDescription(..), TestSuite(..), TestType(..) )
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Compiler ( Compiler(..) )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, PathTemplateVariable(..)
    , substPathTemplate , toPathTemplate, PathTemplate )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.Setup ( TestFlags(..), TestFilter(..), fromFlag )
import Distribution.Simple.Utils ( notice )
import Distribution.TestSuite
    ( Test, Result(..), ImpureTestable(..), TestOptions(..), TestSuiteLog(..)
    , suitePassed, suiteFailed, suiteError )
import Distribution.Text
import Distribution.Verbosity ( Verbosity )
import Distribution.Version ( Version(..), withinVersion, withinRange )
import Distribution.System ( buildPlatform )

import Control.Exception ( bracket )
import Control.Monad ( when, foldM, liftM, unless )
import Data.Char ( toUpper )
import System.Directory
    ( createDirectoryIfMissing, doesFileExist, getCurrentDirectory
    , removeFile )
import System.Environment ( getEnvironment )
import System.Exit ( ExitCode(..), exitFailure, exitSuccess, exitWith )
import System.FilePath ( (</>), (<.>) )
import System.IO
    ( Handle, hClose, hGetContents, hPutStrLn, IOMode(..)
    , openTempFile, withFile )
import System.Process ( runProcess, waitForProcess )

-- |Perform the \"@.\/setup test@\" action.
test :: PackageDescription  -- ^information from the .cabal file
     -> LocalBuildInfo      -- ^information from the configure step
     -> TestFlags           -- ^flags sent to test
     -> IO ()
test pkg_descr lbi flags = do
    let verbosity = fromFlag $ testVerbosity flags
        template = fromFlag $ testLogFile flags
        distPref = fromFlag $ testDistPref flags
        testLogDir = distPref </> "test"
        withinVersion1 = flip withinRange $ withinVersion $ Version [1,0] []

        doTest logs suite = do
            notice verbosity $ "Test suite " ++ testName suite ++ ": RUNNING..."
            let appendFiles = map logFile logs
                run = runTestExe pkg_descr testLogDir
                named = namedTestLog pkg_descr lbi testLogDir template
            case testType suite of
                ExeTest v _ | withinVersion1 v -> do
                    let cmd = buildDir lbi </> testName suite
                            </> testName suite <.> exeExtension
                    testLog <- run cmd Nothing $ \exit out -> do
                        let r = case exit of
                                ExitSuccess -> Pass
                                ExitFailure _ -> Fail out
                            local = (localTestSuiteLog lbi suite)
                                { suiteTests = [(testName suite, r)] }
                        writeLog appendFiles $ named local

                    printResults flags testLog
                    return $ testLog : logs

                LibTest v _ | withinVersion1 v -> do
                    let cmd = buildDir lbi </> stubName suite
                            </> stubName suite <.> exeExtension
                    testLog <- withTestLog
                        (localTestSuiteLog lbi suite) (distPref </> "test")
                        $ \hIn -> run cmd (Just hIn) $ \_ out ->
                            writeLog appendFiles $ named $ read out

                    printResults flags testLog
                    return $ testLog : logs
                _ -> do
                    let dieLog = (localTestSuiteLog lbi suite)
                            { suiteTests = [(testName suite, Error
                                $ "No support for running test suite "
                                ++ "type: " ++ show (disp $ testType suite))]
                            }
                    return $ dieLog : logs

    createDirectoryIfMissing True testLogDir
    let totalSuites = length $ testSuites pkg_descr
    notice verbosity $ "Running " ++ show totalSuites ++ " test suites..."
    allOk <- foldM doTest [] (testSuites pkg_descr) >>= printSummary verbosity
    unless allOk exitFailure

-- | Print a summary to the console after all test suites have been run
-- indicating the number of successful test suites and cases.  Returns 'True' if
-- all test suites passed and 'False' otherwise.
printSummary :: Verbosity -> [TestSuiteLog] -> IO Bool
printSummary verbosity testLogs = do
    notice verbosity $ show passedSuites ++ " of " ++ show totalSuites
        ++ " test suites (" ++ show passedCases ++ " of "
        ++ show totalCases ++ " test cases) passed."
    return $ passedSuites == totalSuites
    where cases = map snd $ concatMap suiteTests testLogs
          passedCases = length $ filter (== Pass) cases
          totalCases = length cases
          passedSuites = length $ filter suitePassed testLogs
          totalSuites = length testLogs

-- | Print a description of the test suite results on the console.  The output
-- depends on the verbosity and test filter settings.
printResults :: TestFlags -> TestSuiteLog -> IO ()
printResults flags testLog = do
    when shouldPrint $ mapM_ printTestCase $ suiteTests testLog
    notice verb $ "Test suite " ++ suiteName testLog ++ ": " ++ resStr
    where
        verb = fromFlag $ testVerbosity flags
        filt = fromFlag $ testFilter flags
        notPassed = not $ suitePassed testLog
        shouldPrint = (filt > Summary && notPassed) || filt > Failures
        resStr = map toUpper (resultString testLog)
        printTestCase (n, r) = notice verb
            $ "Test case " ++ n ++ ": " ++ show r

-- | Creates a temporary file for writing, allowing an 'IO' action to access the
-- file.  The temporary file is deleted after the action runs.
withTempFile :: FilePath -- ^ directory where temporary file will be located
             -> FilePath -- ^ template for temporary file name
             -> ((FilePath, Handle) -> IO a)
             -- ^ action using the temporary file
             -> IO a
withTempFile dir template go = bracket
    (openTempFile dir template)
    (\(actualPath, h) -> do
        hClose h
        tempExists <- doesFileExist actualPath
        when tempExists $ removeFile actualPath
    )
    go

-- | Writes a 'TestSuiteLog' to file.  If the file is to be appended, reads the
-- list of existing 'TestSuiteLog's from the file and overwrites the file with
-- a new list, the new 'TestSuiteLog' being appended to the list.  Otherwise,
-- overwrites the file with a list containing a single element, the new
-- 'TestSuiteLog'.
writeLog :: [FilePath]
         -- ^ list of files to append, rather than overwrite
         -> TestSuiteLog -- ^ test suite log to write to file
         -> IO TestSuiteLog
writeLog files testLog = do
    if (logFile testLog) `elem` files
        then do withFile (logFile testLog) ReadMode $ \hIn -> do
                    existing <- liftM read $ hGetContents hIn
                    withFile (logFile testLog) WriteMode
                        $ \hOut -> hPutStrLn hOut $ show $ existing ++ [testLog]
        else withFile (logFile testLog) WriteMode
                $ \h -> hPutStrLn h $ show [testLog]
    return testLog

resultString :: TestSuiteLog -> String
resultString l | suiteError l = "error"
               | suiteFailed l = "fail"
               | otherwise = "pass"

-- | Runs an executable test suite, such as an @exitcode-stdio@ test suite, or
-- the test stub for a detailed test suite.
runTestExe :: PackageDescription
           -- ^ the 'PackageDescription' of the package this test belongs to;
           -- used to set the shell environment so the executable can find
           -- the package's data files.
           -> FilePath
           -- ^ the directory where temporary files should be located
           -> FilePath -- ^ the command to invoke
           -> Maybe Handle
           -- ^ maybe the handle to give the child process for standard input
           -> (ExitCode -> String -> IO a)
           -- ^ an 'IO' action on the process exit code and output
           -> IO a
runTestExe pkg_descr dir cmd mH go = do
    shellEnv <- dataDirEnv pkg_descr
    let prefix = "cabal-test-" <.> "log"
    withTempFile dir prefix $ \(outFile, hOut) -> do
        proc <- runProcess cmd [] Nothing shellEnv mH (Just hOut) (Just hOut)
        exit <- waitForProcess proc
        hClose hOut
        withFile outFile ReadMode $ (>>= go exit) . hGetContents

-- | The environment used for variable substitution for test log
-- 'PathTemplate's.
testPathTemplateEnv :: PackageDescription
                    -- ^ the description of the package owning the test suite,
                    -- used to resolve the package name
                    -> LocalBuildInfo
                    -- ^ used to resolve the platform information
                    -> TestSuiteLog
                    -- ^ used to resolve the test suite name and result
                    -> [(PathTemplateVariable, PathTemplate)]
testPathTemplateEnv pkg_descr lbi l =
    initialPathTemplateEnv (package pkg_descr) (compilerId $ compiler lbi)
        ++  [ (TestSuiteNameVar, toPathTemplate $ suiteName l)
            , (TestSuiteResultVar, toPathTemplate $ resultString l)
            ]

-- | Modifies a 'TestSuiteLog' to include the final log file name based on
-- a 'PathTemplate'.
namedTestLog :: PackageDescription
             -- ^ used to resolve the log file name
             -> LocalBuildInfo
             -- ^ used to resolve the log file name
             -> FilePath
             -- ^ directory where log file should be located
             -> PathTemplate
             -- ^ template for naming log file
             -> TestSuiteLog
             -- ^ 'TestSuiteLog' to be modified
             -> TestSuiteLog
namedTestLog pkg_descr lbi testDir template testLog =
    let env = testPathTemplateEnv pkg_descr lbi testLog
        logPath = fromPathTemplate $ substPathTemplate env template
    in testLog
        { logFile = testDir </> logPath }

-- | Writes data to a temporary file for reading e.g., as standard input by a
-- child process.
withTestLog :: Show a => a -- ^ options
            -> FilePath -- ^ temporary file name template
            -> (Handle -> IO a) -- ^ action requiring options file
            -> IO a
withTestLog opts dir go =
    withTempFile dir ("cabal-stub-" <.> "opts")
        $ \(optsFile, hOpts) -> do
            hPutStrLn hOpts (show opts)
            hClose hOpts
            withFile optsFile ReadMode go

-- | The filename of the source file for the stub executable associated with a
-- library 'TestSuite'.
stubFilePath :: TestSuite -> FilePath
stubFilePath t = stubName t <.> "hs"

-- | The name of the stub executable associated with a library 'TestSuite'.
stubName :: TestSuite -> FilePath
stubName t = testName t ++ "Stub"

-- | Write the source file for a library 'TestSuite' stub executable.
writeSimpleTestStub :: TestSuite    -- ^ library 'TestSuite' for which a stub
                                    -- is being created
                    -> FilePath     -- ^ path to directory where stub source
                                    -- should be located
                    -> IO ()
writeSimpleTestStub t dir = do
    createDirectoryIfMissing True dir
    let filename = dir </> stubFilePath t
        LibTest _ m = testType t
    withFile filename WriteMode $ \h -> do
        hPutStrLn h $ simpleTestStub m

-- | Source code for library test suite stub executable
simpleTestStub :: ModuleName -> String
simpleTestStub m = unlines
    [ "module Main ( main ) where"
    , "import Control.Monad ( liftM )"
    , "import Distribution.Simple.Test ( runTests )"
    , "import " ++ show (disp m) ++ " ( tests )"
    , "main :: IO ()"
    , "main = runTests tests"
    ]

-- | The test runner for library 'TestSuite' stub executables.  Runs a list of
-- 'Test's.  The 'TestStubOptions' are read from @stdin@ by the stub executable.
runTests :: [Test] -> IO ()
runTests tests = do
    localLog <- liftM read getContents
    testLog <- foldM go localLog tests
    putStrLn $ show testLog
    when (suiteError testLog) $ exitWith $ ExitFailure 2
    when (suiteFailed testLog) $ exitWith $ ExitFailure 1
    exitSuccess
    where
        go :: TestSuiteLog -> Test -> IO TestSuiteLog
        go l t = do
            r <- defaultOptions t >>= runM t
            return $ l { suiteTests = suiteTests l ++ [(name t, r)] }

-- | Returns the shell environment for correctly running a test suite with
-- in-place data files.
dataDirEnv :: PackageDescription -> IO (Maybe [(String, String)])
dataDirEnv pkg_descr = do
    pwd <- getCurrentDirectory
    existingEnv <- getEnvironment
    return $ Just $ (n ++ "_datadir", pwd </> dataDir pkg_descr) : existingEnv
    where PackageName n' = pkgName $ package pkg_descr
          n = map (\c -> if c == '-' then '_' else c) n'

-- | Fills in a 'TestSuiteLog' with data from the 'LocalBuildInfo' and the
-- 'TestSuite' (all fields except the log file name and test case results).
localTestSuiteLog :: LocalBuildInfo -> TestSuite -> TestSuiteLog
localTestSuiteLog lbi suite = TestSuiteLog
    { suitePkg = package $ localPkgDescr lbi
    , suiteCompiler = compilerId $ compiler lbi
    , suitePlatform = buildPlatform
    , suiteName = testName suite
    , suiteTests = []
    , logFile = []
    }