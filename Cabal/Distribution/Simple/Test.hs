-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Test
-- Copyright   :  Thomas Tuegel 2010
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point into testing a built package. It performs the
-- \"@.\/setup test@\" action. It runs test suites designated in the package
-- description and reports on the results.

module Distribution.Simple.Test
    ( test
    ) where

import qualified Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(buildable)
         , TestSuite(..)
         , TestSuiteInterface(..), testType, hasTests )
import Distribution.Simple.Compiler ( compilerInfo )
import Distribution.Simple.Hpc ( markupPackage )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, substPathTemplate
    , PathTemplate )
import qualified Distribution.Simple.LocalBuildInfo as LBI
    ( LocalBuildInfo(..) )
import Distribution.Simple.Setup ( TestFlags(..), fromFlag, configCoverage )
import Distribution.Simple.UserHooks ( Args )
import qualified Distribution.Simple.Test.ExeV10 as ExeV10
import qualified Distribution.Simple.Test.LibV09 as LibV09
import Distribution.Simple.Test.Log
import Distribution.Simple.Utils ( die, notice )
import Distribution.TestSuite ( Result(..) )
import Distribution.Text

import Control.Monad ( when, unless, filterM )
import System.Directory
    ( createDirectoryIfMissing, doesFileExist, getDirectoryContents
    , removeFile )
import System.Exit ( ExitCode(..), exitFailure, exitWith )
import System.FilePath ( (</>) )

-- |Perform the \"@.\/setup test@\" action.
test :: Args                    -- ^positional command-line arguments
     -> PD.PackageDescription   -- ^information from the .cabal file
     -> LBI.LocalBuildInfo      -- ^information from the configure step
     -> TestFlags               -- ^flags sent to test
     -> IO ()
test args pkg_descr lbi flags = do
    let verbosity = fromFlag $ testVerbosity flags
        machineTemplate = fromFlag $ testMachineLog flags
        distPref = fromFlag $ testDistPref flags
        testLogDir = distPref </> "test"
        testNames = args
        pkgTests = PD.testSuites pkg_descr
        enabledTests = [ t | t <- pkgTests
                           , PD.testEnabled t
                           , PD.buildable (PD.testBuildInfo t) ]

        doTest :: (PD.TestSuite, Maybe TestSuiteLog) -> IO TestSuiteLog
        doTest (suite, _) =
            case PD.testInterface suite of
              PD.TestSuiteExeV10 _ _ ->
                  ExeV10.runTest pkg_descr lbi flags suite

              PD.TestSuiteLibV09 _ _ ->
                  LibV09.runTest pkg_descr lbi flags suite

              _ -> return TestSuiteLog
                  { testSuiteName = PD.testName suite
                  , testLogs = TestLog
                      { testName = PD.testName suite
                      , testOptionsReturned = []
                      , testResult =
                          Error $ "No support for running test suite type: "
                                  ++ show (disp $ PD.testType suite)
                      }
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

    let isCoverageEnabled = fromFlag $ configCoverage $ LBI.configFlags lbi
    when isCoverageEnabled $
        markupPackage verbosity lbi distPref (display $ PD.package pkg_descr) $
            map fst testsToRun

    unless allOk exitFailure

packageLogPath :: PathTemplate
               -> PD.PackageDescription
               -> LBI.LocalBuildInfo
               -> FilePath
packageLogPath template pkg_descr lbi =
    fromPathTemplate $ substPathTemplate env template
    where
        env = initialPathTemplateEnv
                (PD.package pkg_descr) (LBI.pkgKey lbi)
                (compilerInfo $ LBI.compiler lbi) (LBI.hostPlatform lbi)
