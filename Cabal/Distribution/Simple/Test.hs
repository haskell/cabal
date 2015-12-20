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

module Distribution.Simple.Test (action, test) where

import qualified Distribution.PackageDescription as PD
    ( PackageDescription(..), BuildInfo(..), TestSuite(..)
    , TestSuiteInterface(..), testType, hasTests )
import Distribution.Simple.Compiler ( compilerInfo )
import Distribution.Simple.Configure ( findDistPrefOrDefault )
import Distribution.Simple.Hpc ( markupPackage )
import Distribution.Simple.InstallDirs
    ( fromPathTemplate, initialPathTemplateEnv, substPathTemplate
    , PathTemplate )
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Reconfigure (Reconfigure)
import Distribution.Simple.Setup
    ( BuildFlags(..), ConfigFlags(..), TestFlags(..)
    , configCoverage, fromFlag, fromFlagOrDefault, toFlag )
import Distribution.Simple.UserHooks ( Args )
import qualified Distribution.Simple.Test.ExeV10 as ExeV10
import qualified Distribution.Simple.Test.LibV09 as LibV09
import Distribution.Simple.Test.Log
import Distribution.Simple.Utils ( die, notice, warn )
import Distribution.TestSuite ( Result(..) )
import Distribution.Text

import Control.Monad ( when, unless, filterM )
import Data.Maybe ( mapMaybe )
import System.Directory
    ( createDirectoryIfMissing, doesFileExist, getDirectoryContents
    , removeFile )
import System.Exit ( ExitCode(..), exitFailure, exitWith )
import System.FilePath ( (</>) )

action :: Reconfigure ConfigFlags LocalBuildInfo
       -> (BuildFlags -> Args -> IO ())
       -> (LocalBuildInfo -> TestFlags -> Args -> IO ())
       -> (BuildFlags, TestFlags) -> Args -> IO ()
action reconfigure build test_ (buildFlags, testFlags) args = do
    distPref <- findDistPrefOrDefault (testDistPref testFlags)
    let testFlags' = testFlags { testDistPref = toFlag distPref }
        verbosity = fromFlag (testVerbosity testFlags')

        withEnabledTests fs
          | fromFlagOrDefault False (configTests fs) = Nothing
          | otherwise = Just (enableTests fs, "enabling tests")
        enableTests fs = fs { configTests = toFlag True }

    -- Get the persistent build config.
    -- If necessary, reconfigure with tests enabled.
    lbi <- reconfigure [withEnabledTests] verbosity distPref

    let buildFlags' = buildFlags
                      { buildDistPref = testDistPref testFlags
                      , buildVerbosity = testVerbosity testFlags
                      }
        buildArgs_ -- extra arguments to 'build'
          | null args = tests -- build all tests, but *only* tests
          | otherwise = args -- build only named tests
        tests = mapMaybe nameTestsOnly $ LBI.pkgComponents pkgDescr
        pkgDescr = LBI.localPkgDescr lbi
        nameTestsOnly =
            LBI.foldComponent
              (const Nothing)
              (const Nothing)
              (\t ->
                if PD.buildable (PD.testBuildInfo t)
                  then Just (PD.testName t)
                else Nothing)
              (const Nothing)

    if null tests
       then warn verbosity "test: no buildable test suites"
      else do
        -- Ensure that all requested test suites are built.
        build buildFlags' buildArgs_

        test_ lbi testFlags' args

-- |Perform the \"@.\/setup test@\" action.
test :: Args                    -- ^positional command-line arguments
     -> PD.PackageDescription   -- ^information from the .cabal file
     -> LocalBuildInfo          -- ^information from the configure step
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
                (PD.package pkg_descr) (LBI.localComponentId lbi)
                (compilerInfo $ LBI.compiler lbi) (LBI.hostPlatform lbi)
