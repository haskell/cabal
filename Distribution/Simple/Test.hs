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
    , TestSuiteLog(..)
    , suitePassed, suiteFailed, suiteError
    ) where

import Distribution.Compat.TempFile ( openTempFile )
import Distribution.ModuleName ( ModuleName )
import Distribution.Package
    ( PackageIdentifier(..), PackageName(..), PackageId )
import qualified Distribution.PackageDescription as PD
    ( PackageDescription(..), TestSuite(..), TestType(..), testVersion1 )
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Compiler ( Compiler(..), CompilerId )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, PathTemplateVariable(..)
    , substPathTemplate , toPathTemplate, PathTemplate )
import qualified Distribution.Simple.LocalBuildInfo as LBI
    ( LocalBuildInfo(..) )
import Distribution.Simple.Setup ( TestFlags(..), TestFilter(..), fromFlag )
import Distribution.Simple.Utils ( notice )
import qualified Distribution.TestSuite as TestSuite
    ( Test, Result(..), ImpureTestable(..), TestOptions(..), Options(..) )
import Distribution.Text
import Distribution.Verbosity ( normal, Verbosity )
import Distribution.System ( buildPlatform, Platform )

import Control.Exception ( bracket )
import Control.Monad ( when, liftM, unless )
import Data.Char ( toUpper )
import Data.Monoid ( mempty )
import System.Directory
    ( copyFile, createDirectoryIfMissing, doesFileExist, getCurrentDirectory
    , removeFile )
import System.Environment ( getEnvironment )
import System.Exit ( ExitCode(..), exitFailure, exitSuccess, exitWith )
import System.FilePath ( (</>), (<.>) )
import System.IO
    ( Handle, hClose, hGetContents, hPutStrLn, IOMode(..), withFile )
import System.Process ( runProcess, waitForProcess )

-- | A data structure used for logging test suite results itemized by test case.
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

data TestSuiteLog = TestSuiteLog
    { name :: String
    , cases :: [Case]
    , logFile :: FilePath    -- Path to human-readable log file
    }
    deriving (Read, Show, Eq)

data Case = Case
    { caseName :: String
    , caseOptions :: TestSuite.Options
    , caseResult :: TestSuite.Result
    }
    deriving (Read, Show, Eq)

-- | From a 'TestSuiteLog', determine if the test suite passed.
suitePassed :: TestSuiteLog -> Bool
suitePassed = all (== TestSuite.Pass) . map caseResult . cases

-- | From a 'TestSuiteLog', determine if the test suite failed.
suiteFailed :: TestSuiteLog -> Bool
suiteFailed = any isFail . map caseResult . cases
    where isFail x = case x of TestSuite.Fail _ -> True; _ -> False

-- | From a 'TestSuiteLog', determine if the test suite encountered errors.
suiteError :: TestSuiteLog -> Bool
suiteError = any isError . map caseResult . cases
    where isError x = case x of TestSuite.Error _ -> True; _ -> False

-- |Perform the \"@.\/setup test@\" action.
test :: PD.PackageDescription   -- ^information from the .cabal file
     -> LBI.LocalBuildInfo      -- ^information from the configure step
     -> TestFlags               -- ^flags sent to test
     -> IO ()
test pkg_descr lbi flags = do
    let verbosity = fromFlag $ testVerbosity flags
        filt = fromFlag $ testFilter flags
        humanTemplate = fromFlag $ testHumanLog flags
        machineTemplate = fromFlag $ testMachineLog flags
        distPref = fromFlag $ testDistPref flags
        testLogDir = distPref </> "test"

        doTest :: PD.TestSuite -> IO TestSuiteLog
        doTest suite = do
            notice verbosity
                $ "Test suite " ++ PD.testName suite ++ ": RUNNING..."
            let resolve = resolveTestPathTemplate humanTemplate pkg_descr lbi
                run = runTestExe pkg_descr testLogDir resolve
            case PD.testType suite of
                PD.ExeTest v _ | PD.testVersion1 v -> do
                    let cmd = LBI.buildDir lbi </> PD.testName suite
                            </> PD.testName suite <.> exeExtension
                    testLog <- run cmd Nothing $ \exit -> do
                        let r = case exit of
                                ExitSuccess -> TestSuite.Pass
                                ExitFailure c -> TestSuite.Fail
                                    $ "exit code: " ++ show c
                        return $ TestSuiteLog
                            { name = PD.testName suite
                            , cases = [Case (PD.testName suite) mempty r]
                            , logFile = ""
                            }

                    summarizeSuite verbosity testLog
                    return testLog

                PD.LibTest v _ | PD.testVersion1 v -> do
                    let cmd = LBI.buildDir lbi </> stubName suite
                            </> stubName suite <.> exeExtension
                    testLog <- withTempFile testLogDir "cabal-test-.log"
                        $ \(tempFile, hTemp) -> do
                            hPutStrLn hTemp $ show $ TestSuiteLog
                                { name = PD.testName suite
                                , cases = []
                                , logFile = tempFile
                                }
                            hClose hTemp
                            withFile tempFile ReadMode
                                $ \hIn -> run cmd (Just hIn) $ \_ -> do
                                    hClose hIn
                                    withFile tempFile ReadMode $
                                        (>>= (return $!) . read) . hGetContents

                    mapM_ (summarizeCase verbosity filt) $ cases testLog
                    summarizeSuite verbosity testLog
                    return testLog
                _ -> do
                    let dieLog = TestSuiteLog
                            { name = PD.testName suite
                            , cases = [Case (PD.testName suite) mempty
                                $ TestSuite.Error $ "No support for running "
                                ++ "test suite type: "
                                ++ show (disp $ PD.testType suite)]
                            , logFile = ""
                            }
                    return dieLog

    createDirectoryIfMissing True testLogDir
    let totalSuites = length $ PD.testSuites pkg_descr
    notice verbosity $ "Running " ++ show totalSuites ++ " test suites..."
    suites <- mapM doTest $ PD.testSuites pkg_descr
    let packageLog = (localPackageLog pkg_descr lbi) { testSuites = suites }
        packageLogFile = (</>) testLogDir
            $ resolveTestPathTemplate machineTemplate pkg_descr lbi
            $ TestSuiteLog { name = "", cases = [], logFile = "" }
    allOk <- summarizePackage verbosity packageLog
    withFile packageLogFile WriteMode $ ($ show packageLog) . hPutStrLn
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

-- | Print to the console a summary of a single test case's result, suppressing
-- output for certain verbosity or test filter levels.
summarizeCase :: Verbosity -> TestFilter -> Case -> IO ()
summarizeCase verb filt t =
    when shouldPrint $ notice verb $ "Test case " ++ caseName t
        ++ ": " ++ show (caseResult t)
    where  shouldPrint =
            (filt > Summary && caseResult t /= TestSuite.Pass)
            || filt > Failures

-- | Print a description of the test suite results on the console, suppressing
-- output for certain verbosity or test filter levels.
summarizeSuite :: Verbosity -> TestSuiteLog -> IO ()
summarizeSuite verb testLog =
    notice verb $ "Test suite " ++ name testLog ++ ": " ++ resStr
    where resStr = map toUpper (resultString testLog)

-- | Creates a temporary file for writing, allowing an 'IO' action to access the
-- file.  The temporary file is deleted after the action runs.
withTempFile :: FilePath -- ^ directory where temporary file will be located
             -> FilePath -- ^ template for temporary file name
             -> ((FilePath, Handle) -> IO a)
             -- ^ action using the temporary file
             -> IO a
withTempFile dir template go =
    bracket (openTempFile dir template) delete go
    where
        delete (actualPath, handle) = do
            hClose handle
            tempExists <- doesFileExist actualPath
            when tempExists $ removeFile actualPath

-- | Runs an executable test suite, such as an @exitcode-stdio@ test suite, or
-- the test stub for a detailed test suite.
runTestExe :: PD.PackageDescription
           -- ^ the 'PackageDescription' of the package this test belongs to;
           -- used to set the shell environment so the executable can find
           -- the package's data files.
           -> FilePath
           -- ^ the directory where temporary files should be located
           -> (TestSuiteLog -> FilePath)
           -- ^ 'resolveTestPathTemplate' partially applied to the
           -- 'PackageDescription' and 'LocalBuildInfo'
           -> FilePath -- ^ the command to invoke
           -> Maybe Handle
           -- ^ maybe the handle to give the child process for standard input
           -> (ExitCode -> IO TestSuiteLog)
           -- ^ an 'IO' action on the process exit code and output
           -> IO TestSuiteLog
runTestExe pkg_descr dir logPath cmd mH go = do
    -- Determine the shell environment to set so the test executable can find
    -- package data files.
    pwd <- getCurrentDirectory
    existingEnv <- getEnvironment
    let dataDirModule = packageName ++ "_datadir"
        dataDirPath = pwd </> PD.dataDir pkg_descr
        PackageName n = pkgName $ PD.package pkg_descr
        packageName = map (\c -> if c == '-' then '_' else c) n
        shellEnv = Just $ (dataDirModule, dataDirPath) : existingEnv

    withTempFile dir ("cabal-test-" <.> "log") $ \(outFile, hOut) -> do
        proc <- runProcess cmd [] Nothing shellEnv mH (Just hOut) (Just hOut)
        exit <- waitForProcess proc
        hClose hOut
        suiteLog <- go exit
        let finalFile = dir </> logPath suiteLog
        copyFile outFile finalFile
        return $ suiteLog { logFile = finalFile }

resultString :: TestSuiteLog -> String
resultString l | suiteError l = "error"
               | suiteFailed l = "fail"
               | otherwise = "pass"

resolveTestPathTemplate :: PathTemplate
                        -> PD.PackageDescription
                        -> LBI.LocalBuildInfo
                        -> TestSuiteLog
                        -> FilePath
resolveTestPathTemplate template pkg_descr lbi testLog =
    fromPathTemplate $ substPathTemplate env template
    where
        env = initialPathTemplateEnv
                (PD.package pkg_descr) (compilerId $ LBI.compiler lbi)
                ++  [ (TestSuiteNameVar, toPathTemplate $ name testLog)
                    , (TestSuiteResultVar, result)
                    ]
        result = toPathTemplate $ resultString testLog

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
        PD.LibTest _ m = PD.testType t
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
runTests :: [TestSuite.Test] -> IO ()
runTests tests = do
    testLogIn <- liftM read getContents
    cases' <- mapM go tests
    let testLog = testLogIn { cases = cases'}
    withFile (logFile testLog) WriteMode $ \h ->
        hPutStrLn h $ show testLog
    when (suiteError testLog) $ exitWith $ ExitFailure 2
    when (suiteFailed testLog) $ exitWith $ ExitFailure 1
    exitSuccess
    where
        go :: TestSuite.Test -> IO Case
        go t = do
            o <- TestSuite.defaultOptions t
            r <- TestSuite.runM t o
            let ret = Case
                    { caseName = TestSuite.name t
                    , caseOptions = o
                    , caseResult = r
                    }
            summarizeCase normal All ret
            return ret