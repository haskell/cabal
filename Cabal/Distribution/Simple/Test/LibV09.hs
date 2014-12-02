module Distribution.Simple.Test.LibV09
       ( runTest
         -- Test stub
       , simpleTestStub
       , stubFilePath, stubMain, stubName, stubWriteLog
       , writeSimpleTestStub
       ) where

import Distribution.Compat.CreatePipe ( createPipe )
import Distribution.Compat.Environment ( getEnvironment )
import Distribution.Compat.TempFile ( openTempFile )
import Distribution.ModuleName ( ModuleName )
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Build.PathsModule ( pkgPathEnvVar )
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Compiler ( compilerInfo )
import Distribution.Simple.Hpc ( markupTest, tixDir, tixFilePath )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, PathTemplateVariable(..)
    , substPathTemplate , toPathTemplate, PathTemplate )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Setup
    ( TestFlags(..), TestShowDetails(..), fromFlag, configCoverage )
import Distribution.Simple.Test.Log
import Distribution.Simple.Utils ( die, notice, rawSystemIOWithEnv )
import Distribution.TestSuite
import Distribution.Text
import Distribution.Verbosity ( normal )

import Control.Exception ( bracket )
import Control.Monad ( when, unless )
import Data.Maybe ( mapMaybe )
import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
    , getCurrentDirectory, removeDirectoryRecursive, removeFile
    , setCurrentDirectory )
import System.Exit ( ExitCode(..), exitWith )
import System.FilePath ( (</>), (<.>) )
import System.IO ( hClose, hGetContents, hPutStr )

runTest :: PD.PackageDescription
        -> LBI.LocalBuildInfo
        -> TestFlags
        -> PD.TestSuite
        -> IO TestSuiteLog
runTest pkg_descr lbi flags suite = do
    let isCoverageEnabled = fromFlag $ configCoverage $ LBI.configFlags lbi

    pwd <- getCurrentDirectory
    existingEnv <- getEnvironment

    let cmd = LBI.buildDir lbi </> stubName suite
                  </> stubName suite <.> exeExtension
    -- Check that the test executable exists.
    exists <- doesFileExist cmd
    unless exists $ die $ "Error: Could not find test program \"" ++ cmd
                          ++ "\". Did you build the package first?"

    -- Remove old .tix files if appropriate.
    unless (fromFlag $ testKeepTix flags) $ do
        let tDir = tixDir distPref $ PD.testName suite
        exists' <- doesDirectoryExist tDir
        when exists' $ removeDirectoryRecursive tDir

    -- Create directory for HPC files.
    createDirectoryIfMissing True $ tixDir distPref $ PD.testName suite

    -- Write summary notices indicating start of test suite
    notice verbosity $ summarizeSuiteStart $ PD.testName suite

    suiteLog <- bracket openCabalTemp deleteIfExists $ \tempLog -> do

        (rIn, wIn) <- createPipe
        (rOut, wOut) <- createPipe

        -- Prepare standard input for test executable
        --appendFile tempInput $ show (tempInput, PD.testName suite)
        hPutStr wIn $ show (tempLog, PD.testName suite)
        hClose wIn

        -- Run test executable
        _ <- do let opts = map (testOption pkg_descr lbi suite) $ testOptions flags
                    dataDirPath = pwd </> PD.dataDir pkg_descr
                    tixFile = pwd </> tixFilePath distPref (PD.testName suite)
                    pkgPathEnv = (pkgPathEnvVar pkg_descr "datadir", dataDirPath)
                               : existingEnv
                    shellEnv = [("HPCTIXFILE", tixFile) | isCoverageEnabled]
                             ++ pkgPathEnv
                rawSystemIOWithEnv verbosity cmd opts Nothing (Just shellEnv)
                                   -- these handles are closed automatically
                                   (Just rIn) (Just wOut) (Just wOut)

        -- Generate final log file name
        let finalLogName l = testLogDir
                             </> testSuiteLogPath
                                 (fromFlag $ testHumanLog flags) pkg_descr lbi
                                 (testSuiteName l) (testLogs l)
        -- Generate TestSuiteLog from executable exit code and a machine-
        -- readable test log
        suiteLog <- fmap ((\l -> l { logFile = finalLogName l }) . read)
                    $ readFile tempLog

        -- Write summary notice to log file indicating start of test suite
        appendFile (logFile suiteLog) $ summarizeSuiteStart $ PD.testName suite

        -- Append contents of temporary log file to the final human-
        -- readable log file
        logText <- hGetContents rOut
        appendFile (logFile suiteLog) logText

        -- Write end-of-suite summary notice to log file
        appendFile (logFile suiteLog) $ summarizeSuiteFinish suiteLog

        -- Show the contents of the human-readable log file on the terminal
        -- if there is a failure and/or detailed output is requested
        let details = fromFlag $ testShowDetails flags
            whenPrinting = when $ (details > Never)
                && (not (suitePassed $ testLogs suiteLog) || details == Always)
                && verbosity >= normal
        whenPrinting $ putStr $ unlines $ lines logText

        return suiteLog

    -- Write summary notice to terminal indicating end of test suite
    notice verbosity $ summarizeSuiteFinish suiteLog

    when isCoverageEnabled $
        markupTest verbosity lbi distPref (display $ PD.package pkg_descr) suite

    return suiteLog
  where
    deleteIfExists file = do
        exists <- doesFileExist file
        when exists $ removeFile file

    testLogDir = distPref </> "test"
    openCabalTemp = do
        (f, h) <- openTempFile testLogDir $ "cabal-test-" <.> "log"
        hClose h >> return f

    distPref = fromFlag $ testDistPref flags
    verbosity = fromFlag $ testVerbosity flags

-- TODO: This is abusing the notion of a 'PathTemplate'.  The result isn't
-- necessarily a path.
testOption :: PD.PackageDescription
           -> LBI.LocalBuildInfo
           -> PD.TestSuite
           -> PathTemplate
           -> String
testOption pkg_descr lbi suite template =
    fromPathTemplate $ substPathTemplate env template
  where
    env = initialPathTemplateEnv
          (PD.package pkg_descr) (LBI.pkgKey lbi)
          (compilerInfo $ LBI.compiler lbi) (LBI.hostPlatform lbi) ++
          [(TestSuiteNameVar, toPathTemplate $ PD.testName suite)]

-- Test stub ----------

-- | The name of the stub executable associated with a library 'TestSuite'.
stubName :: PD.TestSuite -> FilePath
stubName t = PD.testName t ++ "Stub"

-- | The filename of the source file for the stub executable associated with a
-- library 'TestSuite'.
stubFilePath :: PD.TestSuite -> FilePath
stubFilePath t = stubName t <.> "hs"

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
    , "import Distribution.Simple.Test.LibV09 ( stubMain )"
    , "import " ++ show (disp m) ++ " ( tests )"
    , "main :: IO ()"
    , "main = stubMain tests"
    ]

-- | Main function for test stubs. Once, it was written directly into the stub,
-- but minimizing the amount of code actually in the stub maximizes the number
-- of detectable errors when Cabal is compiled.
stubMain :: IO [Test] -> IO ()
stubMain tests = do
    (f, n) <- fmap read getContents
    dir <- getCurrentDirectory
    results <- tests >>= stubRunTests
    setCurrentDirectory dir
    stubWriteLog f n results

-- | The test runner used in library "TestSuite" stub executables.  Runs a list
-- of 'Test's.  An executable calling this function is meant to be invoked as
-- the child of a Cabal process during @.\/setup test@.  A 'TestSuiteLog',
-- provided by Cabal, is read from the standard input; it supplies the name of
-- the test suite and the location of the machine-readable test suite log file.
-- Human-readable log information is written to the standard output for capture
-- by the calling Cabal process.
stubRunTests :: [Test] -> IO TestLogs
stubRunTests tests = do
    logs <- mapM stubRunTests' tests
    return $ GroupLogs "Default" logs
  where
    stubRunTests' (Test t) = do
        l <- run t >>= finish
        summarizeTest normal Always l
        return l
      where
        finish (Finished result) =
            return TestLog
                { testName = name t
                , testOptionsReturned = defaultOptions t
                , testResult = result
                }
        finish (Progress _ next) = next >>= finish
    stubRunTests' g@(Group {}) = do
        logs <- mapM stubRunTests' $ groupTests g
        return $ GroupLogs (groupName g) logs
    stubRunTests' (ExtraOptions _ t) = stubRunTests' t
    maybeDefaultOption opt =
        maybe Nothing (\d -> Just (optionName opt, d)) $ optionDefault opt
    defaultOptions testInst = mapMaybe maybeDefaultOption $ options testInst

-- | From a test stub, write the 'TestSuiteLog' to temporary file for the calling
-- Cabal process to read.
stubWriteLog :: FilePath -> String -> TestLogs -> IO ()
stubWriteLog f n logs = do
    let testLog = TestSuiteLog { testSuiteName = n, testLogs = logs, logFile = f }
    writeFile (logFile testLog) $ show testLog
    when (suiteError logs) $ exitWith $ ExitFailure 2
    when (suiteFailed logs) $ exitWith $ ExitFailure 1
    exitWith ExitSuccess
