module Distribution.Simple.Test.ExeV10
       ( runTest
       ) where

import Distribution.Compat.CreatePipe ( createPipe, tee )
import Distribution.Compat.Environment ( getEnvironment )
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Build.PathsModule ( pkgPathEnvVar )
import Distribution.Simple.BuildPaths ( exeExtension )
import Distribution.Simple.Compiler ( Compiler(..) )
import Distribution.Simple.Hpc ( markupTest, tixDir, tixFilePath )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, PathTemplateVariable(..)
    , substPathTemplate , toPathTemplate, PathTemplate )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Setup ( TestFlags(..), TestShowDetails(..), fromFlag )
import Distribution.Simple.Test.Log
import Distribution.Simple.Utils ( die, notice, rawSystemIOWithEnv )
import Distribution.TestSuite
import Distribution.Text
import Distribution.Verbosity ( normal )

import Control.Monad ( when, unless )
import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
    , getCurrentDirectory, removeDirectoryRecursive )
import System.Exit ( ExitCode(..) )
import System.FilePath ( (</>), (<.>) )
import System.IO ( hGetContents, stdout )

runTest :: PD.PackageDescription
        -> LBI.LocalBuildInfo
        -> TestFlags
        -> PD.TestSuite
        -> IO TestSuiteLog
runTest pkg_descr lbi flags suite = do
    pwd <- getCurrentDirectory
    existingEnv <- getEnvironment

    let cmd = LBI.buildDir lbi </> PD.testName suite
                  </> PD.testName suite <.> exeExtension
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

    -- Run test executable
    (rLog, wLog) <- createPipe
    let opts = map (testOption pkg_descr lbi suite)
                   (testOptions flags)
        dataDirPath = pwd </> PD.dataDir pkg_descr
        shellEnv = (pkgPathEnvVar pkg_descr "datadir", dataDirPath)
                   : ("HPCTIXFILE", (</>) pwd
                       $ tixFilePath distPref $ PD.testName suite)
                   : existingEnv

    (rOut, wOut) <- createPipe
    let outHandles | details == Streaming = [(stdout, False)]
                   | otherwise = []
    tee rOut $ (wLog, True) : outHandles
    exit <- rawSystemIOWithEnv verbosity cmd opts Nothing (Just shellEnv)
                               -- these handles are automatically closed
                               Nothing (Just wOut) (Just wOut)

    -- Generate TestSuiteLog from executable exit code and a machine-
    -- readable test log
    let suiteLog = buildLog exit

    -- Write summary notice to log file indicating start of test suite
    appendFile (logFile suiteLog) $ summarizeSuiteStart $ PD.testName suite

    -- Append contents of temporary log file to the final human-
    -- readable log file
    logText <- hGetContents rLog
    appendFile (logFile suiteLog) logText

    -- Write end-of-suite summary notice to log file
    appendFile (logFile suiteLog) $ summarizeSuiteFinish suiteLog

    -- Show the contents of the human-readable log file on the terminal
    -- if there is a failure and/or detailed output is requested
    let
        whenPrinting = when $ (details > Never)
            && (not (suitePassed $ testLogs suiteLog) || details == Always)
            && verbosity >= normal -- verbosity overrides show-details
            && details /= Streaming -- If streaming, we already printed the log
    whenPrinting $ putStr $ unlines $ lines logText

    -- Write summary notice to terminal indicating end of test suite
    notice verbosity $ summarizeSuiteFinish suiteLog

    markupTest verbosity lbi distPref
        (display $ PD.package pkg_descr) suite

    return suiteLog
  where
    distPref = fromFlag $ testDistPref flags
    verbosity = fromFlag $ testVerbosity flags
    details = fromFlag $ testShowDetails flags
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
        in TestSuiteLog
                { testSuiteName = n
                , testLogs = l
                , logFile =
                    testLogDir
                    </> testSuiteLogPath (fromFlag $ testHumanLog flags)
                                         pkg_descr lbi n l
                }

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
          (PD.package pkg_descr) (compilerId $ LBI.compiler lbi)
          (LBI.hostPlatform lbi) ++
          [(TestSuiteNameVar, toPathTemplate $ PD.testName suite)]
