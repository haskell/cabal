{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Simple.Test.LibV09
  ( runTest
  -- Test stub
  , simpleTestStub
  , stubFilePath
  , stubMain
  , stubName
  , stubWriteLog
  , writeSimpleTestStub
  ) where

import Distribution.Compat.Prelude
import Distribution.Types.UnqualComponentName
import Prelude ()

import Distribution.Compat.Environment
import Distribution.Compat.Internal.TempFile
import Distribution.Compat.Process (proc)
import Distribution.ModuleName
import qualified Distribution.PackageDescription as PD
import Distribution.Pretty
import Distribution.Simple.Build.PathsModule
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.Errors
import Distribution.Simple.Hpc
import Distribution.Simple.InstallDirs
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Test
import Distribution.Simple.Test.Log
import Distribution.Simple.Utils
import Distribution.System
import Distribution.TestSuite
import qualified Distribution.Types.LocalBuildInfo as LBI
import Distribution.Utils.Path
import Distribution.Verbosity

import qualified Control.Exception as CE
import qualified Data.ByteString.Lazy as LBS
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , removeDirectoryRecursive
  , removeFile
  , setCurrentDirectory
  )
import System.IO (hClose, hPutStr)
import qualified System.Process as Process

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

  let mbWorkDir = LBI.mbWorkDirLBI lbi
  existingEnv <- getEnvironment

  let cmd =
        interpretSymbolicPath mbWorkDir (LBI.buildDir lbi)
          </> stubName suite
          </> stubName suite <.> exeExtension (LBI.hostPlatform lbi)
      tDir = i $ tixDir distPref way
  -- Check that the test executable exists.
  exists <- doesFileExist cmd
  unless exists $
    dieWithException verbosity $
      Couldn'tFindTestProgLibV09 cmd

  -- Remove old .tix files if appropriate.
  unless (fromFlag $ testKeepTix flags) $ do
    exists' <- doesDirectoryExist tDir
    when exists' $ removeDirectoryRecursive tDir

  -- Create directory for HPC files.
  createDirectoryIfMissing True tDir

  -- Write summary notices indicating start of test suite
  notice verbosity $ summarizeSuiteStart testName'

  suiteLog <- CE.bracket openCabalTemp deleteIfExists $ \tempLog -> do
    -- Run test executable
    let opts = map (testOption pkg_descr lbi suite) $ testOptions flags
        rawDataDirPath = PD.dataDir pkg_descr
        dataDirPath
          | null $ getSymbolicPath rawDataDirPath =
              i sameDirectory
          | otherwise =
              i rawDataDirPath
        tixFile = i $ tixFilePath distPref way testName'
        pkgPathEnv =
          (pkgPathEnvVar pkg_descr "datadir", dataDirPath)
            : existingEnv
        shellEnv =
          [("HPCTIXFILE", tixFile) | isCoverageEnabled]
            ++ pkgPathEnv
    -- Add (DY)LD_LIBRARY_PATH if needed
    shellEnv' <-
      if LBI.withDynExe lbi
        then do
          let (Platform _ os) = LBI.hostPlatform lbi
          paths <- LBI.depLibraryPaths True False lbi clbi
          cpath <- canonicalizePath $ i $ LBI.componentBuildDir lbi clbi
          return (addLibraryPath os (cpath : paths) shellEnv)
        else return shellEnv
    let (cmd', opts') = case testWrapper flags of
          Flag path -> (path, cmd : opts)
          NoFlag -> (cmd, opts)

    -- TODO: this setup is broken,
    -- if the test output is too big, we will deadlock.
    (rOut, wOut) <- Process.createPipe
    (exitcode, logText) <- rawSystemProcAction
      verbosity
      (proc cmd' opts')
        { Process.env = Just shellEnv'
        , Process.std_in = Process.CreatePipe
        , Process.std_out = Process.UseHandle wOut
        , Process.std_err = Process.UseHandle wOut
        }
      $ \mIn _ _ -> do
        let wIn = fromCreatePipe mIn
        hPutStr wIn $ show (tempLog, PD.testName suite)
        hClose wIn

        -- Append contents of temporary log file to the final human-
        -- readable log file
        logText <- LBS.hGetContents rOut
        -- Force the IO manager to drain the test output pipe
        _ <- evaluate (force logText)
        return logText
    unless (exitcode == ExitSuccess) $
      debug verbosity $
        cmd ++ " returned " ++ show exitcode

    -- Generate final log file name
    let finalLogName l =
          interpretSymbolicPath mbWorkDir testLogDir
            </> testSuiteLogPath
              (fromFlag $ testHumanLog flags)
              pkg_descr
              lbi
              (unUnqualComponentName $ testSuiteName l)
              (testLogs l)
    -- Generate TestSuiteLog from executable exit code and a machine-
    -- readable test log
    suiteLog <-
      fmap
        ( \s ->
            (\l -> l{logFile = finalLogName l})
              . fromMaybe (error $ "panic! read @TestSuiteLog " ++ show s)
              $ readMaybe s -- TODO: eradicateNoParse
        )
        $ readFile tempLog

    -- Write summary notice to log file indicating start of test suite
    appendFile (logFile suiteLog) $ summarizeSuiteStart testName'

    LBS.appendFile (logFile suiteLog) logText

    -- Write end-of-suite summary notice to log file
    appendFile (logFile suiteLog) $ summarizeSuiteFinish suiteLog

    -- Show the contents of the human-readable log file on the terminal
    -- if there is a failure and/or detailed output is requested
    let details = fromFlag $ testShowDetails flags
        whenPrinting =
          when $
            (details > Never)
              && (not (suitePassed $ testLogs suiteLog) || details == Always)
              && verbosity >= normal
    whenPrinting $ do
      LBS.putStr logText
      putChar '\n'

    return suiteLog

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
    i = LBI.interpretSymbolicPathLBI lbi
    common = testCommonFlags flags
    testName' = unUnqualComponentName $ PD.testName suite

    deleteIfExists file = do
      exists <- doesFileExist file
      when exists $ removeFile file

    testLogDir = distPref </> makeRelativePathEx "test"
    openCabalTemp = do
      (f, h) <- openTempFile (i testLogDir) $ "cabal-test-" <.> "log"
      hClose h >> return f

    distPref = fromFlag $ setupDistPref common
    verbosity = fromFlag $ setupVerbosity common

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

-- Test stub ----------

-- | The filename of the source file for the stub executable associated with a
-- library 'TestSuite'.
stubFilePath :: PD.TestSuite -> FilePath
stubFilePath t = stubName t <.> "hs"

-- | Write the source file for a library 'TestSuite' stub executable.
writeSimpleTestStub
  :: PD.TestSuite
  -- ^ library 'TestSuite' for which a stub
  -- is being created
  -> FilePath
  -- ^ path to directory where stub source
  -- should be located
  -> IO ()
writeSimpleTestStub t dir = do
  createDirectoryIfMissing True dir
  let filename = dir </> stubFilePath t
      m = case PD.testInterface t of
        PD.TestSuiteLibV09 _ m' -> m'
        _ -> error "writeSimpleTestStub: invalid TestSuite passed"
  writeFile filename $ simpleTestStub m

-- | Source code for library test suite stub executable
simpleTestStub :: ModuleName -> String
simpleTestStub m =
  unlines
    [ "module Main ( main ) where"
    , "import Distribution.Simple.Test.LibV09 ( stubMain )"
    , "import " ++ show (pretty m) ++ " ( tests )"
    , "main :: IO ()"
    , "main = stubMain tests"
    ]

-- | Main function for test stubs. Once, it was written directly into the stub,
-- but minimizing the amount of code actually in the stub maximizes the number
-- of detectable errors when Cabal is compiled.
stubMain :: IO [Test] -> IO ()
stubMain tests = do
  (f, n) <- fmap (\s -> fromMaybe (error $ "panic! read " ++ show s) $ readMaybe s) getContents -- TODO: eradicateNoParse
  dir <- getCurrentDirectory
  results <- (tests >>= stubRunTests) `CE.catch` errHandler
  setCurrentDirectory dir
  stubWriteLog f n results
  where
    errHandler :: CE.SomeException -> IO TestLogs
    errHandler e = case CE.fromException e of
      Just CE.UserInterrupt -> CE.throwIO e
      _ ->
        return $
          TestLog
            { testName = "Cabal test suite exception"
            , testOptionsReturned = []
            , testResult = Error $ show e
            }

-- | The test runner used in library "TestSuite" stub executables.  Runs a list
-- of 'Test's.  An executable calling this function is meant to be invoked as
-- the child of a Cabal process during @.\/setup test@.  A 'TestSuiteLog',
-- provided by Cabal, is read from the standard input; it supplies the name of
-- the test suite and the location of the machine-readable test suite log file.
-- Human-readable log information is written to the standard output for capture
-- by the calling Cabal process.
stubRunTests :: [Test] -> IO TestLogs
stubRunTests tests = do
  logs <- traverse stubRunTests' tests
  return $ GroupLogs "Default" logs
  where
    stubRunTests' (Test t) = do
      l <- run t >>= finish
      summarizeTest normal Always l
      return l
      where
        finish (Finished result) =
          return
            TestLog
              { testName = name t
              , testOptionsReturned = defaultOptions t
              , testResult = result
              }
        finish (Progress _ next) = next >>= finish
    stubRunTests' g@(Group{}) = do
      logs <- traverse stubRunTests' $ groupTests g
      return $ GroupLogs (groupName g) logs
    stubRunTests' (ExtraOptions _ t) = stubRunTests' t
    maybeDefaultOption opt =
      maybe Nothing (\d -> Just (optionName opt, d)) $ optionDefault opt
    defaultOptions testInst = mapMaybe maybeDefaultOption $ options testInst

-- | From a test stub, write the 'TestSuiteLog' to temporary file for the calling
-- Cabal process to read.
stubWriteLog :: FilePath -> UnqualComponentName -> TestLogs -> IO ()
stubWriteLog f n logs = do
  let testLog = TestSuiteLog{testSuiteName = n, testLogs = logs, logFile = f}
  writeFile (logFile testLog) $ show testLog
  when (suiteError logs) $ exitWith $ ExitFailure 2
  when (suiteFailed logs) $ exitWith $ ExitFailure 1
  exitSuccess
