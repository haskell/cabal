{-# LANGUAGE RecordWildCards #-}

module Distribution.Simple.Test.ExeV10 ( runTest ) where

import Control.Concurrent (forkIO)
import Control.Monad ( unless, void, when )
import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
    , getCurrentDirectory, removeDirectoryRecursive )
import System.Exit ( ExitCode(..) )
import System.FilePath ( (</>), (<.>) )
import System.IO ( hGetContents, hPutStr, stdout, stderr )

import Distribution.Compat.CreatePipe
import Distribution.Compat.Environment

import Distribution.Flag
import qualified Distribution.PackageDescription as PD
import Distribution.System
import Distribution.TestSuite
import Distribution.Text
import Distribution.Verbosity

import Distribution.Simple.Build.PathsModule
import Distribution.Simple.BuildPaths
import Distribution.Simple.Command.Test
import Distribution.Simple.Compiler
import Distribution.Simple.Hpc
import Distribution.Simple.InstallDirs
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Setup ( configCoverage )
import Distribution.Simple.Test.Log
import Distribution.Simple.Utils

runTest :: PD.PackageDescription
        -> LBI.LocalBuildInfo
        -> TestConfig
        -> PD.TestSuite
        -> IO TestSuiteLog
runTest pkg_descr lbi (TestConfig {..}) suite = do
    let isCoverageEnabled = fromFlag $ configCoverage $ LBI.configFlags lbi
        way = guessWay lbi
        tixDir_ = tixDir distPref way $ PD.testName suite

    pwd <- getCurrentDirectory
    existingEnv <- getEnvironment

    let cmd = LBI.buildDir lbi </> PD.testName suite
              </> PD.testName suite <.> exeExtension
    -- Check that the test executable exists.
    exists <- doesFileExist cmd
    unless exists $ die $ "Error: Could not find test program \"" ++ cmd
                          ++ "\". Did you build the package first?"

    -- Remove old .tix files if appropriate.
    unless keepTix $ do
        exists' <- doesDirectoryExist tixDir_
        when exists' $ removeDirectoryRecursive tixDir_

    -- Create directory for HPC files.
    createDirectoryIfMissing True tixDir_

    -- Write summary notices indicating start of test suite
    notice verbosity $ summarizeSuiteStart $ PD.testName suite

    (wOut, wErr, logText) <- case details of
        Direct -> return (stdout, stderr, "")
        _ -> do
            (rOut, wOut) <- createPipe

            -- Read test executable's output lazily (returns immediately)
            logText <- hGetContents rOut
            -- Force the IO manager to drain the test output pipe
            void $ forkIO $ length logText `seq` return ()

            -- '--show-details=streaming': print the log output in another thread
            when (details == Streaming) $ void $ forkIO $ hPutStr stdout logText

            return (wOut, wOut, logText)

    -- Run the test executable
    let opts = map (testOption pkg_descr lbi suite) options
        dataDirPath = pwd </> PD.dataDir pkg_descr
        tixFile = pwd </> tixFilePath distPref way (PD.testName suite)
        pkgPathEnv = (pkgPathEnvVar pkg_descr "datadir", dataDirPath)
                   : existingEnv
        shellEnv = [("HPCTIXFILE", tixFile) | isCoverageEnabled] ++ pkgPathEnv

    -- Add (DY)LD_LIBRARY_PATH if needed
    shellEnv' <- if LBI.withDynExe lbi
                    then do let (Platform _ os) = LBI.hostPlatform lbi
                                clbi = LBI.getComponentLocalBuildInfo lbi
                                         (LBI.CTestName (PD.testName suite))
                            paths <- LBI.depLibraryPaths True False lbi clbi
                            return (addLibraryPath os paths shellEnv)
                    else return shellEnv

    exit <- rawSystemIOWithEnv verbosity cmd opts Nothing (Just shellEnv')
                               -- these handles are automatically closed
                               Nothing (Just wOut) (Just wErr)

    -- Generate TestSuiteLog from executable exit code and a machine-
    -- readable test log.
    let suiteLog = buildLog exit

    -- Write summary notice to log file indicating start of test suite
    appendFile (logFile suiteLog) $ summarizeSuiteStart $ PD.testName suite

    -- Append contents of temporary log file to the final human-
    -- readable log file
    appendFile (logFile suiteLog) logText

    -- Write end-of-suite summary notice to log file
    appendFile (logFile suiteLog) $ summarizeSuiteFinish suiteLog

    -- Show the contents of the human-readable log file on the terminal
    -- if there is a failure and/or detailed output is requested
    let whenPrinting = when $
            ( details == Always ||
              details == Failures && not (suitePassed $ testLogs suiteLog))
            -- verbosity overrides show-details
            && verbosity >= normal
    whenPrinting $ putStr $ unlines $ lines logText

    -- Write summary notice to terminal indicating end of test suite
    notice verbosity $ summarizeSuiteFinish suiteLog

    when isCoverageEnabled $
        markupTest verbosity lbi distPref (display $ PD.package pkg_descr) suite

    return suiteLog
  where
    details = showDetails
    testLogDir = distPref </> "test"

    buildLog exit =
        let r = case exit of
                    ExitSuccess -> Pass
                    ExitFailure c -> Fail $ "exit code: " ++ show c
            n = PD.testName suite
            l = TestLog
                { testName = n
                , testOptionsReturned = []
                , testResult = r
                }
            lf = testLogDir </> testSuiteLogPath humanLog pkg_descr lbi n l
        in TestSuiteLog
                { testSuiteName = n
                , testLogs = l
                , logFile = lf
                }

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
          (PD.package pkg_descr) (LBI.localUnitId lbi)
          (compilerInfo $ LBI.compiler lbi) (LBI.hostPlatform lbi) ++
          [(TestSuiteNameVar, toPathTemplate $ PD.testName suite)]
