{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

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

import Distribution.Compat.Prelude
import Prelude ()

import qualified Distribution.PackageDescription as PD
import Distribution.Pretty
import Distribution.Simple.Compiler
import Distribution.Simple.Hpc
import Distribution.Simple.InstallDirs
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Setup.Test
import qualified Distribution.Simple.Test.ExeV10 as ExeV10
import qualified Distribution.Simple.Test.LibV09 as LibV09
import Distribution.Simple.Test.Log
import Distribution.Simple.UserHooks
import Distribution.Simple.Utils
import Distribution.TestSuite
import qualified Distribution.Types.LocalBuildInfo as LBI
import Distribution.Types.UnqualComponentName
import Distribution.Utils.Path

import Distribution.Simple.Configure (getInstalledPackagesById)
import Distribution.Simple.Errors
import Distribution.Simple.Register (internalPackageDBPath)
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Config
import Distribution.Types.ExposedModule
import Distribution.Types.InstalledPackageInfo (InstalledPackageInfo (libraryDirs), exposedModules)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo (..))
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getDirectoryContents
  , removeFile
  )

-- | Perform the \"@.\/setup test@\" action.
test
  :: Args
  -- ^ positional command-line arguments
  -> PD.PackageDescription
  -- ^ information from the .cabal file
  -> LBI.LocalBuildInfo
  -- ^ information from the configure step
  -> TestFlags
  -- ^ flags sent to test
  -> IO ()
test args pkg_descr lbi0 flags = do
  let common = testCommonFlags flags
      verbosity = fromFlag $ setupVerbosity common
      distPref = fromFlag $ setupDistPref common
      mbWorkDir = flagToMaybe $ setupWorkingDir common
      i = interpretSymbolicPath mbWorkDir -- See Note [Symbolic paths] in Distribution.Utils.Path
      machineTemplate = fromFlag $ testMachineLog flags
      testLogDir = distPref </> makeRelativePathEx "test"
      testNames = args
      pkgTests = PD.testSuites pkg_descr
      enabledTests = LBI.enabledTestLBIs pkg_descr lbi
      -- We must add the internalPkgDB to the package database stack to lookup
      -- the path to HPC dirs of libraries local to this package
      internalPkgDB = LBI.interpretSymbolicPathLBI lbi $ internalPackageDBPath lbi distPref
      lbi = lbi0{withPackageDB = withPackageDB lbi0 ++ [SpecificPackageDB internalPkgDB]}

      doTest
        :: HPCMarkupInfo
        -> ( (PD.TestSuite, LBI.ComponentLocalBuildInfo)
           , Maybe TestSuiteLog
           )
        -> IO TestSuiteLog
      doTest hpcMarkupInfo ((suite, clbi), _) =
        case PD.testInterface suite of
          PD.TestSuiteExeV10 _ _ ->
            ExeV10.runTest pkg_descr lbi clbi hpcMarkupInfo flags suite
          PD.TestSuiteLibV09 _ _ ->
            LibV09.runTest pkg_descr lbi clbi hpcMarkupInfo flags suite
          _ ->
            return
              TestSuiteLog
                { testSuiteName = PD.testName suite
                , testLogs =
                    TestLog
                      { testName = unUnqualComponentName $ PD.testName suite
                      , testOptionsReturned = []
                      , testResult =
                          Error $
                            "No support for running test suite type: "
                              ++ show (pretty $ PD.testType suite)
                      }
                , logFile = ""
                }

  unless (PD.hasTests pkg_descr) $ do
    notice verbosity "Package has no test suites."
    exitSuccess

  when (PD.hasTests pkg_descr && null enabledTests) $
    dieWithException verbosity NoTestSuitesEnabled

  testsToRun <- case testNames of
    [] -> return $ zip enabledTests $ repeat Nothing
    names -> for names $ \tName ->
      let testMap = zip enabledNames enabledTests
          enabledNames = map (PD.testName . fst) enabledTests
          allNames = map PD.testName pkgTests
          tCompName = mkUnqualComponentName tName
       in case lookup tCompName testMap of
            Just t -> return (t, Nothing)
            _
              | tCompName `elem` allNames ->
                  dieWithException verbosity $ TestNameDisabled tName
              | otherwise -> dieWithException verbosity $ NoSuchTest tName

  createDirectoryIfMissing True $ i testLogDir

  -- Delete ordinary files from test log directory.
  getDirectoryContents (i testLogDir)
    >>= filterM doesFileExist . map (i testLogDir </>)
    >>= traverse_ removeFile

  -- We configured the unit-ids of libraries we should cover in our coverage
  -- report at configure time into the local build info. At build time, we built
  -- the hpc artifacts into the extraCompilationArtifacts directory, which, at
  -- install time, is copied into the ghc-pkg database files.
  -- Now, we get the path to the HPC artifacts and exposed modules of each
  -- library by querying the package database keyed by unit-id:
  let coverageFor =
        nub $
          fromFlagOrDefault [] (configCoverageFor (configFlags lbi))
            <> extraCoverageFor lbi
  ipkginfos <- getInstalledPackagesById verbosity lbi MissingCoveredInstalledLibrary coverageFor
  let ( concat -> pathsToLibsArtifacts
        , concat -> libsModulesToInclude
        ) =
          unzip $
            map
              ( \ip ->
                  ( map ((</> coerceSymbolicPath extraCompilationArtifacts) . makeSymbolicPath) $ libraryDirs ip
                  , map exposedName $ exposedModules ip
                  )
              )
              ipkginfos
      hpcMarkupInfo = HPCMarkupInfo{pathsToLibsArtifacts, libsModulesToInclude}

  let totalSuites = length testsToRun
  notice verbosity $ "Running " ++ show totalSuites ++ " test suites..."
  suites <- traverse (doTest hpcMarkupInfo) testsToRun
  let packageLog = (localPackageLog pkg_descr lbi){testSuites = suites}
      packageLogFile =
        i testLogDir
          </> packageLogPath machineTemplate pkg_descr lbi
  allOk <- summarizePackage verbosity packageLog
  writeFile packageLogFile $ show packageLog

  when (LBI.testCoverage lbi) $
    markupPackage verbosity hpcMarkupInfo lbi distPref pkg_descr $
      map (fst . fst) testsToRun

  unless allOk exitFailure

packageLogPath
  :: PathTemplate
  -> PD.PackageDescription
  -> LBI.LocalBuildInfo
  -> FilePath
packageLogPath template pkg_descr lbi =
  fromPathTemplate $ substPathTemplate env template
  where
    env =
      initialPathTemplateEnv
        (PD.package pkg_descr)
        (LBI.localUnitId lbi)
        (compilerInfo $ LBI.compiler lbi)
        (LBI.hostPlatform lbi)
