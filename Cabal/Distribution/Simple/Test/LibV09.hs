module Distribution.Simple.Test.LibV09
       ( runTest
         -- Test stub
       , simpleTestStub
       , stubFilePath, stubMain, stubName, stubWriteLog
       , writeSimpleTestStub
       , testSuiteLibV09AsLibAndExe 
       ) where

import Distribution.Compat.CreatePipe
import Distribution.Compat.Environment
import Distribution.Compat.Internal.TempFile
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription hiding (options)
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Build.PathsModule
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.Configure
import Distribution.Simple.Hpc
import Distribution.Simple.InstallDirs
import Distribution.Simple.LocalBuildInfo hiding (substPathTemplate)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Register
import Distribution.Simple.Setup
import Distribution.Simple.Test.Log hiding
    ( package, testSuites, compiler, testName )
import qualified Distribution.Simple.Test.Log as TestLog
import Distribution.Simple.Utils
import Distribution.System
import Distribution.TestSuite
import Distribution.Text
import Distribution.Verbosity

import Control.Exception ( bracket )
import Control.Monad ( when, unless )
import qualified Data.Map as Map
import Data.Maybe ( mapMaybe )
import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
    , getCurrentDirectory, removeDirectoryRecursive, removeFile
    , setCurrentDirectory )
import System.Exit ( ExitCode(..), exitWith )
import System.FilePath ( (</>), (<.>) )
import System.IO ( hClose, hGetContents, hPutStr )
import System.Process (StdStream(..), waitForProcess)

runTest :: PD.PackageDescription
        -> LBI.LocalBuildInfo
        -> TestFlags
        -> PD.TestSuite
        -> IO TestSuiteLog
runTest pkg_descr lbi flags suite = do
    let isCoverageEnabled = fromFlag $ configCoverage $ LBI.configFlags lbi
        way = guessWay lbi

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
        let tDir = tixDir distPref way $ PD.testName suite
        exists' <- doesDirectoryExist tDir
        when exists' $ removeDirectoryRecursive tDir

    -- Create directory for HPC files.
    createDirectoryIfMissing True $ tixDir distPref way $ PD.testName suite

    -- Write summary notices indicating start of test suite
    notice verbosity $ summarizeSuiteStart $ PD.testName suite

    suiteLog <- bracket openCabalTemp deleteIfExists $ \tempLog -> do

        (rOut, wOut) <- createPipe

        -- Run test executable
        (Just wIn, _, _, process) <- do
                let opts = map (testOption pkg_descr lbi suite) $ testOptions flags
                    dataDirPath = pwd </> PD.dataDir pkg_descr
                    tixFile = pwd </> tixFilePath distPref way (PD.testName suite)
                    pkgPathEnv = (pkgPathEnvVar pkg_descr "datadir", dataDirPath)
                               : existingEnv
                    shellEnv = [("HPCTIXFILE", tixFile) | isCoverageEnabled]
                             ++ pkgPathEnv
                -- Add (DY)LD_LIBRARY_PATH if needed
                shellEnv' <- if LBI.withDynExe lbi
                                then do
                                  let (Platform _ os) = LBI.hostPlatform lbi
                                      clbi = LBI.getComponentLocalBuildInfo
                                                   lbi
                                                   (LBI.CTestName
                                                      (PD.testName suite))
                                      (_, _, _, _, _, _, exeClbi) =
                                          testSuiteLibV09AsLibAndExe
                                              pkg_descr suite clbi lbi
                                              distPref pwd
                                  -- Get the dependencies of the stub
                                  -- executable. They should include the test
                                  -- library itself (#2039).
                                  paths <- LBI.depLibraryPaths
                                             True False lbi exeClbi
                                  return (addLibraryPath os paths shellEnv)
                                else return shellEnv
                createProcessWithEnv verbosity cmd opts Nothing (Just shellEnv')
                                     -- these handles are closed automatically
                                     CreatePipe (UseHandle wOut) (UseHandle wOut)

        hPutStr wIn $ show (tempLog, PD.testName suite)
        hClose wIn

        -- Append contents of temporary log file to the final human-
        -- readable log file
        logText <- hGetContents rOut
        -- Force the IO manager to drain the test output pipe
        length logText `seq` return ()

        exitcode <- waitForProcess process
        unless (exitcode == ExitSuccess) $ do
            debug verbosity $ cmd ++ " returned " ++ show exitcode

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
          (PD.package pkg_descr) (LBI.localUnitId lbi)
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
                { TestLog.testName = name t
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

-- | Translate a lib-style 'TestSuite' component into a lib + exe for building
testSuiteLibV09AsLibAndExe :: PackageDescription
                           -> TestSuite
                           -> ComponentLocalBuildInfo
                           -> LocalBuildInfo
                           -> FilePath
                           -> FilePath
                           -> (PackageDescription,
                               Library, ComponentLocalBuildInfo,
                               LocalBuildInfo,
                               IPI.InstalledPackageInfo,
                               Executable, ComponentLocalBuildInfo)
testSuiteLibV09AsLibAndExe pkg_descr
                     test@TestSuite { testInterface = TestSuiteLibV09 _ m }
                     clbi lbi distPref pwd =
    (pkg, lib, libClbi, lbi, ipi, exe, exeClbi)
  where
    bi  = testBuildInfo test
    lib = Library {
            libName = testName test,
            exposedModules = [ m ],
            reexportedModules = [],
            requiredSignatures = [],
            exposedSignatures = [],
            libExposed     = True,
            libBuildInfo   = bi
          }
    -- This is, like, the one place where we use a CTestName for a library.
    -- Should NOT use library name, since that could conflict!
    PackageIdentifier pkg_name pkg_ver = package pkg_descr
    compat_name = computeCompatPackageName pkg_name (CTestName (testName test))
    compat_key = computeCompatPackageKey (compiler lbi) compat_name pkg_ver (componentUnitId clbi)
    libClbi = LibComponentLocalBuildInfo
                { componentPackageDeps = componentPackageDeps clbi
                , componentLocalName = CLibName (testName test)
                , componentIsPublic = False
                , componentIncludes = componentIncludes clbi
                , componentUnitId = componentUnitId clbi
                , componentCompatPackageName = compat_name
                , componentCompatPackageKey = compat_key
                , componentExposedModules = [IPI.ExposedModule m Nothing]
                }
    pkg = pkg_descr {
            package      = (package pkg_descr) { pkgName = compat_name }
          , buildDepends = targetBuildDepends $ testBuildInfo test
          , executables  = []
          , testSuites   = []
          , libraries    = [lib]
          }
    ipi    = inplaceInstalledPackageInfo pwd distPref pkg (AbiHash "") lib lbi libClbi
    testDir = buildDir lbi </> stubName test
          </> stubName test ++ "-tmp"
    testLibDep = thisPackageVersion $ package pkg
    exe = Executable {
            exeName    = stubName test,
            modulePath = stubFilePath test,
            buildInfo  = (testBuildInfo test) {
                           hsSourceDirs       = [ testDir ],
                           targetBuildDepends = testLibDep
                             : (targetBuildDepends $ testBuildInfo test),
                           targetBuildRenaming = Map.empty
                         }
          }
    -- | The stub executable needs a new 'ComponentLocalBuildInfo'
    -- that exposes the relevant test suite library.
    deps = (IPI.installedUnitId ipi, packageId ipi)
         : (filter (\(_, x) -> let PackageName pname = pkgName x
                               in pname == "Cabal" || pname == "base")
                   (componentPackageDeps clbi))
    exeClbi = ExeComponentLocalBuildInfo {
                -- TODO: this is a hack, but as long as this is unique
                -- (doesn't clobber something) we won't run into trouble
                componentUnitId = mkUnitId (stubName test),
                componentLocalName = CExeName (stubName test),
                componentPackageDeps = deps,
                componentIncludes = zip (map fst deps) (repeat defaultRenaming)
              }
testSuiteLibV09AsLibAndExe _ TestSuite{} _ _ _ _ = error "testSuiteLibV09AsLibAndExe: wrong kind"
