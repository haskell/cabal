{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Simple.Test.ExeV10
  ( runTest
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compat.Environment
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Build.PathsModule
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.Flag
import Distribution.Simple.Hpc
import Distribution.Simple.InstallDirs
import qualified Distribution.Simple.LocalBuildInfo as LBI
  ( ComponentLocalBuildInfo (..)
  , buildDir
  , depLibraryPaths
  )
import Distribution.Simple.Setup.Test
import Distribution.Simple.Test.Log
import Distribution.Simple.Utils
import Distribution.System
import Distribution.TestSuite
import qualified Distribution.Types.LocalBuildInfo as LBI
  ( LocalBuildInfo (..)
  , localUnitId
  , testCoverage
  )
import Distribution.Types.UnqualComponentName
import Distribution.Verbosity

import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((<.>), (</>))
import System.IO (stderr, stdout)
import System.Process (createPipe)

import qualified Data.ByteString.Lazy as LBS
import Distribution.Simple.Errors

runTest
  :: PD.PackageDescription
  -> LBI.LocalBuildInfo
  -> LBI.ComponentLocalBuildInfo
  -> HPCMarkupInfo
  -> TestFlags
  -> PD.TestSuite
  -> IO TestSuiteLog
runTest pkg_descr lbi clbi hpcMarkupInfo flags suite = do
  let isCoverageEnabled = LBI.testCoverage lbi
      way = guessWay lbi
      tixDir_ = tixDir distPref way

  pwd <- getCurrentDirectory
  existingEnv <- getEnvironment

  let cmd =
        LBI.buildDir lbi
          </> testName'
          </> testName' <.> exeExtension (LBI.hostPlatform lbi)
  -- Check that the test executable exists.
  exists <- doesFileExist cmd
  unless exists $
    dieWithException verbosity $
      Couldn'tFindTestProgram cmd

  -- Remove old .tix files if appropriate.
  unless (fromFlag $ testKeepTix flags) $ do
    exists' <- doesDirectoryExist tixDir_
    when exists' $ removeDirectoryRecursive tixDir_

  -- Create directory for HPC files.
  createDirectoryIfMissing True tixDir_

  -- Write summary notices indicating start of test suite
  notice verbosity $ summarizeSuiteStart $ testName'

  -- Run the test executable
  let opts =
        map
          (testOption pkg_descr lbi suite)
          (testOptions flags)
      dataDirPath = pwd </> PD.dataDir pkg_descr
      tixFile = pwd </> tixFilePath distPref way (testName')
      pkgPathEnv =
        (pkgPathEnvVar pkg_descr "datadir", dataDirPath)
          : existingEnv
      shellEnv = [("HPCTIXFILE", tixFile) | isCoverageEnabled] ++ pkgPathEnv

  -- Add (DY)LD_LIBRARY_PATH if needed
  shellEnv' <-
    if LBI.withDynExe lbi
      then do
        let (Platform _ os) = LBI.hostPlatform lbi
        paths <- LBI.depLibraryPaths True False lbi clbi
        return (addLibraryPath os paths shellEnv)
      else return shellEnv

  -- Output logger
  (wOut, wErr, getLogText) <- case details of
    Direct -> return (stdout, stderr, return LBS.empty)
    _ -> do
      (rOut, wOut) <- createPipe

      return $ (,,) wOut wOut $ do
        -- Read test executables' output
        logText <- LBS.hGetContents rOut

        -- '--show-details=streaming': print the log output in another thread
        when (details == Streaming) $ LBS.putStr logText

        -- drain the output.
        evaluate (force logText)

  (exit, logText) <- case testWrapper flags of
    Flag path ->
      rawSystemIOWithEnvAndAction
        verbosity
        path
        (cmd : opts)
        Nothing
        (Just shellEnv')
        getLogText
        -- these handles are automatically closed
        Nothing
        (Just wOut)
        (Just wErr)
    NoFlag ->
      rawSystemIOWithEnvAndAction
        verbosity
        cmd
        opts
        Nothing
        (Just shellEnv')
        getLogText
        -- these handles are automatically closed
        Nothing
        (Just wOut)
        (Just wErr)

  -- Generate TestSuiteLog from executable exit code and a machine-
  -- readable test log.
  let suiteLog = buildLog exit

  -- Write summary notice to log file indicating start of test suite
  appendFile (logFile suiteLog) $ summarizeSuiteStart testName'

  -- Append contents of temporary log file to the final human-
  -- readable log file
  LBS.appendFile (logFile suiteLog) logText

  -- Write end-of-suite summary notice to log file
  appendFile (logFile suiteLog) $ summarizeSuiteFinish suiteLog

  -- Show the contents of the human-readable log file on the terminal
  -- if there is a failure and/or detailed output is requested
  let whenPrinting =
        when $
          ( details == Always
              || details == Failures && not (suitePassed $ testLogs suiteLog)
          )
            -- verbosity overrides show-details
            && verbosity >= normal
  whenPrinting $ do
    LBS.putStr logText
    putChar '\n'

  -- Write summary notice to terminal indicating end of test suite
  notice verbosity $ summarizeSuiteFinish suiteLog

  when isCoverageEnabled $ do
    -- Until #9493 is fixed, we expect cabal-install to pass one dist dir per
    -- library and there being at least one library in the package with the
    -- testsuite.  When it is fixed, we can remove this predicate and allow a
    -- testsuite without a library to cover libraries in other packages of the
    -- same project
    when (null $ PD.allLibraries pkg_descr) $
      dieWithException verbosity TestCoverageSupport

    markupPackage verbosity hpcMarkupInfo lbi distPref pkg_descr [suite]

  return suiteLog
  where
    testName' = unUnqualComponentName $ PD.testName suite

    distPref = fromFlag $ testDistPref flags
    verbosity = fromFlag $ testVerbosity flags
    details = fromFlag $ testShowDetails flags
    testLogDir = distPref </> "test"

    buildLog exit =
      let r = case exit of
            ExitSuccess -> Pass
            ExitFailure c -> Fail $ "exit code: " ++ show c
          -- n = unUnqualComponentName $ PD.testName suite
          l =
            TestLog
              { testName = testName'
              , testOptionsReturned = []
              , testResult = r
              }
       in TestSuiteLog
            { testSuiteName = PD.testName suite
            , testLogs = l
            , logFile =
                testLogDir
                  </> testSuiteLogPath
                    (fromFlag $ testHumanLog flags)
                    pkg_descr
                    lbi
                    testName'
                    l
            }

-- TODO: This is abusing the notion of a 'PathTemplate'.  The result isn't
-- necessarily a path.
testOption
  :: PD.PackageDescription
  -> LBI.LocalBuildInfo
  -> PD.TestSuite
  -> PathTemplate
  -> String
testOption pkg_descr lbi suite template =
  fromPathTemplate $ substPathTemplate env template
  where
    env =
      initialPathTemplateEnv
        (PD.package pkg_descr)
        (LBI.localUnitId lbi)
        (compilerInfo $ LBI.compiler lbi)
        (LBI.hostPlatform lbi)
        ++ [(TestSuiteNameVar, toPathTemplate $ unUnqualComponentName $ PD.testName suite)]
