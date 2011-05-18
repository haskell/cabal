-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Test
-- Copyright   :  Thomas Tuegel 2010
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point into testing a built package. It performs the
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
    , PackageLog(..)
    , TestSuiteLog(..)
    , Case(..)
    , suitePassed, suiteFailed, suiteError
    ) where

import Distribution.Compat.TempFile ( openTempFile )
import Distribution.ModuleName ( ModuleName )
import Distribution.Package
    ( PackageId )
import qualified Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(buildable)
         , TestSuite(..)
         , TestSuiteInterface(..), testType, hasTests )
import Distribution.Simple.Build.PathsModule ( pkgPathEnvVar )
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Compiler ( Compiler(..), CompilerId )
import Distribution.Simple.Hpc ( doHpcMarkup, findTixFiles, tixDir )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, PathTemplateVariable(..)
    , substPathTemplate , toPathTemplate, PathTemplate )
import qualified Distribution.Simple.LocalBuildInfo as LBI
    ( LocalBuildInfo(..) )
import Distribution.Simple.Setup ( TestFlags(..), TestShowDetails(..), fromFlag )
import Distribution.Simple.Utils ( die, notice )
import qualified Distribution.TestSuite as TestSuite
    ( Test, Result(..), ImpureTestable(..), TestOptions(..), Options(..) )
import Distribution.Text
import Distribution.Verbosity ( normal, Verbosity )
import Distribution.System ( buildPlatform, Platform )

import Control.Exception ( bracket )
import Control.Monad ( when, liftM, unless, filterM )
import Data.Char ( toUpper )
import Data.Monoid ( mempty )
import System.Directory
    ( createDirectoryIfMissing, doesFileExist, getCurrentDirectory
    , removeFile, getDirectoryContents )
import System.Environment ( getEnvironment )
import System.Exit ( ExitCode(..), exitFailure, exitWith )
import System.FilePath ( (</>), (<.>) )
import System.IO ( hClose, IOMode(..), openFile )
import System.Process ( runProcess, waitForProcess )

-- | Logs all test results for a package, broken down first by test suite and
-- then by test case.
data PackageLog = PackageLog
    { package :: PackageId
    , compiler :: CompilerId
    , platform :: Platform
    , testSuites :: [TestSuiteLog]
    }
    deriving (Read, Show, Eq)

-- | A 'PackageLog' with package and platform information specified.
localPackageLog :: PD.PackageDescription -> LBI.LocalBuildInfo -> PackageLog
localPackageLog pkg_descr lbi = PackageLog
    { package = PD.package pkg_descr
    , compiler = compilerId $ LBI.compiler lbi
    , platform = buildPlatform
    , testSuites = []
    }

-- | Logs test suite results, itemized by test case.
data TestSuiteLog = TestSuiteLog
    { name :: String
    , cases :: [Case]
    , logFile :: FilePath    -- path to human-readable log file
    }
    deriving (Read, Show, Eq)

data Case = Case
    { caseName :: String
    , caseOptions :: TestSuite.Options
    , caseResult :: TestSuite.Result
    }
    deriving (Read, Show, Eq)

getTestOptions :: TestSuite.Test -> TestSuiteLog -> IO TestSuite.Options
getTestOptions t l =
    case filter ((== TestSuite.name t) . caseName) (cases l) of
        (x:_) -> return $ caseOptions x
        _ -> TestSuite.defaultOptions t

-- | From a 'TestSuiteLog', determine if the test suite passed.
suitePassed :: TestSuiteLog -> Bool
suitePassed = all (== TestSuite.Pass) . map caseResult . cases

-- | From a 'TestSuiteLog', determine if the test suite failed.
suiteFailed :: TestSuiteLog -> Bool
suiteFailed = any isFail . map caseResult . cases
    where isFail (TestSuite.Fail _) = True
          isFail _ = False

-- | From a 'TestSuiteLog', determine if the test suite encountered errors.
suiteError :: TestSuiteLog -> Bool
suiteError = any isError . map caseResult . cases
    where isError (TestSuite.Error _) = True
          isError _ = False

-- | Run a test executable, logging the output and generating the appropriate
-- summary messages.
testController :: TestFlags
               -- ^ flags Cabal was invoked with
               -> PD.PackageDescription
               -- ^ description of package the test suite belongs to
               -> LBI.LocalBuildInfo
               -- ^ information from the configure step
               -> PD.TestSuite
               -- ^ TestSuite being tested
               -> (FilePath -> String)
               -- ^ prepare standard input for test executable
               -> FilePath -- ^ executable name
               -> (ExitCode -> String -> TestSuiteLog)
               -- ^ generator for the TestSuiteLog
               -> (TestSuiteLog -> FilePath)
               -- ^ generator for final human-readable log filename
               -> IO TestSuiteLog
testController flags pkg_descr lbi suite preTest cmd postTest logNamer = do
    let distPref = fromFlag $ testDistPref flags
        verbosity = fromFlag $ testVerbosity flags
        testLogDir = distPref </> "test"
        options = map (testOption pkg_descr lbi suite) $ testOptions flags

    pwd <- getCurrentDirectory
    existingEnv <- getEnvironment
    let dataDirPath = pwd </> PD.dataDir pkg_descr
        shellEnv = Just $ (pkgPathEnvVar pkg_descr "datadir", dataDirPath)
                        : ("HPCTIXDIR", pwd </> tixDir distPref suite)
                        : existingEnv

    bracket (openCabalTemp testLogDir) deleteIfExists $ \tempLog ->
        bracket (openCabalTemp testLogDir) deleteIfExists $ \tempInput -> do

            -- Check that the test executable exists.
            exists <- doesFileExist cmd
            unless exists $ die $ "Error: Could not find test program \"" ++ cmd
                                  ++ "\". Did you build the package first?"

            -- Create directory for HPC files.
            createDirectoryIfMissing True $ tixDir distPref suite

            -- Remove old .tix files if appropriate.
            tixFiles <- findTixFiles distPref suite
            unless (fromFlag $ testKeepTix flags)
                $ mapM_ deleteIfExists tixFiles

            -- Write summary notice to console indicating start of test suite
            notice verbosity $ summarizeSuiteStart $ PD.testName suite

            -- Prepare standard input for test executable
            appendFile tempInput $ preTest tempInput

            -- Run test executable
            exit <- do
              hLog <- openFile tempLog AppendMode
              hIn  <- openFile tempInput ReadMode
              -- these handles get closed by runProcess
              proc <- runProcess cmd options Nothing shellEnv
                        (Just hIn) (Just hLog) (Just hLog)
              waitForProcess proc

            -- Generate TestSuiteLog from executable exit code and a machine-
            -- readable test log
            suiteLog <- fmap (postTest exit $!) $ readFile tempInput

            -- Generate final log file name
            let finalLogName = testLogDir </> logNamer suiteLog
                suiteLog' = suiteLog { logFile = finalLogName }

            -- Write summary notice to log file indicating start of test suite
            appendFile (logFile suiteLog') $ summarizeSuiteStart $ PD.testName suite

            -- Append contents of temporary log file to the final human-
            -- readable log file
            readFile tempLog >>= appendFile (logFile suiteLog')

            -- Write end-of-suite summary notice to log file
            appendFile (logFile suiteLog') $ summarizeSuiteFinish suiteLog'

            -- Show the contents of the human-readable log file on the terminal
            -- if there is a failure and/or detailed output is requested
            let details = fromFlag $ testShowDetails flags
                whenPrinting = when $ (details > Never)
                    && (not (suitePassed suiteLog) || details == Always)
                    && verbosity >= normal
            whenPrinting $ readFile tempLog >>=
                putStr . unlines . lines

            -- Write summary notice to terminal indicating end of test suite
            notice verbosity $ summarizeSuiteFinish suiteLog'

            doHpcMarkup verbosity distPref (display $ PD.package pkg_descr) suite

            return suiteLog'
    where
        deleteIfExists file = do
            exists <- doesFileExist file
            when exists $ removeFile file

        openCabalTemp testLogDir = do
            (f, h) <- openTempFile testLogDir $ "cabal-test-" <.> "log"
            hClose h >> return f


-- |Perform the \"@.\/setup test@\" action.
test :: PD.PackageDescription   -- ^information from the .cabal file
     -> LBI.LocalBuildInfo      -- ^information from the configure step
     -> TestFlags               -- ^flags sent to test
     -> IO ()
test pkg_descr lbi flags = do
    let verbosity = fromFlag $ testVerbosity flags
        humanTemplate = fromFlag $ testHumanLog flags
        machineTemplate = fromFlag $ testMachineLog flags
        distPref = fromFlag $ testDistPref flags
        testLogDir = distPref </> "test"
        testNames = fromFlag $ testList flags
        pkgTests = PD.testSuites pkg_descr
        enabledTests = [ t | t <- pkgTests
                           , PD.testEnabled t
                           , PD.buildable (PD.testBuildInfo t) ]

        doTest :: (PD.TestSuite, Maybe TestSuiteLog) -> IO TestSuiteLog
        doTest (suite, mLog) = do
            let testLogPath = testSuiteLogPath humanTemplate pkg_descr lbi
                go pre cmd post = testController flags pkg_descr lbi suite
                                                 pre cmd post testLogPath
            case PD.testInterface suite of
              PD.TestSuiteExeV10 _ _ -> do
                    let cmd = LBI.buildDir lbi </> PD.testName suite
                            </> PD.testName suite <.> exeExtension
                        preTest _ = ""
                        postTest exit _ =
                            let r = case exit of
                                    ExitSuccess -> TestSuite.Pass
                                    ExitFailure c -> TestSuite.Fail
                                        $ "exit code: " ++ show c
                            in TestSuiteLog
                                { name = PD.testName suite
                                , cases = [Case (PD.testName suite) mempty r]
                                , logFile = ""
                                }
                    go preTest cmd postTest

              PD.TestSuiteLibV09 _ _ -> do
                    let cmd = LBI.buildDir lbi </> stubName suite
                            </> stubName suite <.> exeExtension
                        oldLog = case mLog of
                            Nothing -> TestSuiteLog
                                { name = PD.testName suite
                                , cases = []
                                , logFile = []
                                }
                            Just l -> l
                        preTest f = show $ oldLog { logFile = f }
                        postTest _ = read
                    go preTest cmd postTest

              _ -> return TestSuiteLog
                            { name = PD.testName suite
                            , cases = [Case (PD.testName suite) mempty
                                $ TestSuite.Error $ "No support for running "
                                ++ "test suite type: "
                                ++ show (disp $ PD.testType suite)]
                            , logFile = ""
                            }

    when (not $ PD.hasTests pkg_descr) $ do
        notice verbosity "Package has no test suites."
        exitWith ExitSuccess

    when (PD.hasTests pkg_descr && null enabledTests) $
        die $ "No test suites enabled. Did you remember to configure with "
              ++ "\'--enable-tests\'?"

    testsToRun <- case testNames of
            [] -> return $ zip enabledTests $ repeat Nothing
            names -> flip mapM names $ \tName ->
                let testMap = zip enabledNames enabledTests
                    enabledNames = map PD.testName enabledTests
                    allNames = map PD.testName pkgTests
                in case lookup tName testMap of
                    Just t -> return (t, Nothing)
                    _ | tName `elem` allNames ->
                          die $ "Package configured with test suite "
                                ++ tName ++ " disabled."
                      | otherwise -> die $ "no such test: " ++ tName

    createDirectoryIfMissing True testLogDir

    -- Delete ordinary files from test log directory.
    getDirectoryContents testLogDir
        >>= filterM doesFileExist . map (testLogDir </>)
        >>= mapM_ removeFile

    let totalSuites = length testsToRun
    notice verbosity $ "Running " ++ show totalSuites ++ " test suites..."
    suites <- mapM doTest testsToRun
    let packageLog = (localPackageLog pkg_descr lbi) { testSuites = suites }
        packageLogFile = (</>) testLogDir
            $ packageLogPath machineTemplate pkg_descr lbi
    allOk <- summarizePackage verbosity packageLog
    writeFile packageLogFile $ show packageLog
    unless allOk exitFailure

-- | Print a summary to the console after all test suites have been run
-- indicating the number of successful test suites and cases.  Returns 'True' if
-- all test suites passed and 'False' otherwise.
summarizePackage :: Verbosity -> PackageLog -> IO Bool
summarizePackage verbosity packageLog = do
    let cases' = map caseResult $ concatMap cases $ testSuites packageLog
        passedCases = length $ filter (== TestSuite.Pass) cases'
        totalCases = length cases'
        passedSuites = length $ filter suitePassed $ testSuites packageLog
        totalSuites = length $ testSuites packageLog
    notice verbosity $ show passedSuites ++ " of " ++ show totalSuites
        ++ " test suites (" ++ show passedCases ++ " of "
        ++ show totalCases ++ " test cases) passed."
    return $! passedSuites == totalSuites

-- | Print a summary of a single test case's result to the console, supressing
-- output for certain verbosity or test filter levels.
summarizeCase :: Verbosity -> TestShowDetails -> Case -> IO ()
summarizeCase verbosity details t =
    when shouldPrint $ notice verbosity $ "Test case " ++ caseName t
        ++ ": " ++ show (caseResult t)
    where shouldPrint = (details > Never) && (notPassed || details == Always)
          notPassed = caseResult t /= TestSuite.Pass

-- | Print a summary of the test suite's results on the console, suppressing
-- output for certain verbosity or test filter levels.
summarizeSuiteFinish :: TestSuiteLog -> String
summarizeSuiteFinish testLog = unlines
    [ "Test suite " ++ name testLog ++ ": " ++ resStr
    , "Test suite logged to: " ++ logFile testLog
    ]
    where resStr = map toUpper (resultString testLog)

summarizeSuiteStart :: String -> String
summarizeSuiteStart n = "Test suite " ++ n ++ ": RUNNING...\n"

resultString :: TestSuiteLog -> String
resultString l | suiteError l = "error"
               | suiteFailed l = "fail"
               | otherwise = "pass"

testSuiteLogPath :: PathTemplate
                 -> PD.PackageDescription
                 -> LBI.LocalBuildInfo
                 -> TestSuiteLog
                 -> FilePath
testSuiteLogPath template pkg_descr lbi testLog =
    fromPathTemplate $ substPathTemplate env template
    where
        env = initialPathTemplateEnv
                (PD.package pkg_descr) (compilerId $ LBI.compiler lbi)
                ++  [ (TestSuiteNameVar, toPathTemplate $ name testLog)
                    , (TestSuiteResultVar, result)
                    ]
        result = toPathTemplate $ resultString testLog

-- TODO: This is abusing the notion of a 'PathTemplate'.  The result
-- isn't neccesarily a path.
testOption :: PD.PackageDescription
           -> LBI.LocalBuildInfo
           -> PD.TestSuite
           -> PathTemplate
           -> String
testOption pkg_descr lbi suite template =
    fromPathTemplate $ substPathTemplate env template
  where
    env = initialPathTemplateEnv
          (PD.package pkg_descr) (compilerId $ LBI.compiler lbi) ++
          [(TestSuiteNameVar, toPathTemplate $ PD.testName suite)]

packageLogPath :: PathTemplate
               -> PD.PackageDescription
               -> LBI.LocalBuildInfo
               -> FilePath
packageLogPath template pkg_descr lbi =
    fromPathTemplate $ substPathTemplate env template
    where
        env = initialPathTemplateEnv
                (PD.package pkg_descr) (compilerId $ LBI.compiler lbi)

-- | The filename of the source file for the stub executable associated with a
-- library 'TestSuite'.
stubFilePath :: PD.TestSuite -> FilePath
stubFilePath t = stubName t <.> "hs"

-- | The name of the stub executable associated with a library 'TestSuite'.
stubName :: PD.TestSuite -> FilePath
stubName t = PD.testName t ++ "Stub"

-- | Write the source file for a library 'TestSuite' stub executable.
writeSimpleTestStub :: PD.TestSuite -- ^ library 'TestSuite' for which a stub
                                    -- is being created
                    -> FilePath     -- ^ path to directory where stub source
                                    -- should be located
                    -> IO ()
writeSimpleTestStub t dir = do
    createDirectoryIfMissing True dir
    let filename = dir </> stubFilePath t
        PD.TestSuiteLibV09 _ m = PD.testInterface t
    writeFile filename $ simpleTestStub m

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

-- | The test runner used in library "TestSuite" stub executables.  Runs a list
-- of 'Test's.  An executable calling this function is meant to be invoked as
-- the child of a Cabal process during @.\/setup test@.  A 'TestSuiteLog',
-- provided by Cabal, is read from the standard input; it supplies the name of
-- the test suite and the location of the machine-readable test suite log file.
-- Human-readable log information is written to the standard output for capture
-- by the calling Cabal process.
runTests :: [TestSuite.Test] -> IO ()
runTests tests = do
    testLogIn <- liftM read getContents
    let go :: TestSuite.Test -> IO Case
        go t = do
            o <- getTestOptions t testLogIn
            r <- TestSuite.runM t o
            let ret = Case
                    { caseName = TestSuite.name t
                    , caseOptions = o
                    , caseResult = r
                    }
            summarizeCase normal Always ret
            return ret
    cases' <- mapM go tests
    let testLog = testLogIn { cases = cases'}
    writeFile (logFile testLog) $ show testLog
    when (suiteError testLog) $ exitWith $ ExitFailure 2
    when (suiteFailed testLog) $ exitWith $ ExitFailure 1
    exitWith ExitSuccess
