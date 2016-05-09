{-# LANGUAGE CPP #-}
module Main where

import Distribution.Client.DistDirLayout
import Distribution.Client.ProjectConfig
import Distribution.Client.Config (defaultCabalDir)
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectBuilding
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.Types (GenericReadyPackage(..), installedPackageId)

import Distribution.Package hiding (installedPackageId)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Version
import Distribution.Verbosity
import Distribution.Text

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
import qualified Data.Map as Map
import Control.Monad
import Control.Exception
import System.FilePath
import System.Directory

import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain (testGroup "Integration tests (internal)" tests)

tests :: [TestTree]
tests =
    --TODO: tests for:
    -- * normal success
    -- * dry-run tests with changes
  [ testGroup "Exceptions during discovey and planning" $
    [ testCase "no package"  testExceptionInFindingPackage
    , testCase "no package2" testExceptionInFindingPackage2
    ]
  , testGroup "Exceptions during building (local inplace)" $
    [ testCase "configure"   testExceptionInConfigureStep
    , testCase "build"       testExceptionInBuildStep
--    , testCase "register"   testExceptionInRegisterStep
    ]
    --TODO: need to repeat for packages for the store
  ]

testExceptionInFindingPackage :: Assertion
testExceptionInFindingPackage = do
    BadPackageLocations locs <- expectException "BadPackageLocations" $
      void $ planProject testdir config
    case locs of
      [BadLocGlobEmptyMatch "./*.cabal"] -> return ()
      _ -> assertFailure "expected BadLocGlobEmptyMatch"
    cleanProject testdir
  where
    testdir = "exception/no-pkg"
    config  = mempty


testExceptionInFindingPackage2 :: Assertion
testExceptionInFindingPackage2 = do
    BadPackageLocations locs <- expectException "BadPackageLocations" $
      void $ planProject testdir config
    case locs of
      [BadLocGlobBadMatches "./" [BadLocDirNoCabalFile "."]] -> return ()
      _ -> assertFailure $ "expected BadLocGlobBadMatches, got " ++ show locs
    cleanProject testdir
  where
    testdir = "exception/no-pkg2"
    config  = mempty


testExceptionInConfigureStep :: Assertion
testExceptionInConfigureStep = do
    plan  <- planProject testdir config
    plan' <- executePlan plan
    (_pkga1, failure) <- expectPackageFailed plan' pkgidA1
    case failure of
      ConfigureFailed _str -> return ()
      _ -> assertFailure $ "expected ConfigureFailed, got " ++ show failure 
    cleanProject testdir
  where
    testdir = "exception/configure"
    config  = mempty
    pkgidA1 = PackageIdentifier (PackageName "a") (Version [1] [])


testExceptionInBuildStep :: Assertion
testExceptionInBuildStep = do
    plan  <- planProject testdir config
    plan' <- executePlan plan
    (_pkga1, failure) <- expectPackageFailed plan' pkgidA1
    case failure of
      BuildFailed _str -> return ()
      _ -> assertFailure $ "expected BuildFailed, got " ++ show failure 
  where
    testdir = "exception/build"
    config  = mempty
    pkgidA1 = PackageIdentifier (PackageName "a") (Version [1] [])



---------------------------------
-- Test utils to plan and build
--

planProject :: FilePath -> ProjectConfig -> IO PlanDetails
planProject testdir cliConfig = do
    cabalDir <- defaultCabalDir
    let cabalDirLayout = defaultCabalDirLayout cabalDir

    projectRootDir <- canonicalizePath ("tests" </> "IntegrationTests2"
                                                </> testdir)
    let distDirLayout = defaultDistDirLayout projectRootDir

    -- Clear state between test runs. The state remains if the previous run
    -- ended in an exception (as we leave the files to help with debugging).
    cleanProject testdir

    (elaboratedPlan, elaboratedShared, projectConfig) <-
      rebuildInstallPlan verbosity
                         projectRootDir distDirLayout cabalDirLayout
                         cliConfig

    let targets =
          Map.fromList
            [ (installedPackageId pkg, [BuildDefaultComponents])
            | InstallPlan.Configured pkg <- InstallPlan.toList elaboratedPlan
            , pkgBuildStyle pkg == BuildInplaceOnly ]
        elaboratedPlan' = pruneInstallPlanToTargets targets elaboratedPlan

    (elaboratedPlan'', pkgsBuildStatus) <-
      rebuildTargetsDryRun distDirLayout
                           elaboratedPlan'

    let buildSettings = resolveBuildTimeSettings
                          verbosity cabalDirLayout
                          (projectConfigShared    projectConfig)
                          (projectConfigBuildOnly projectConfig)
                          (projectConfigBuildOnly cliConfig)

    return (distDirLayout,
            elaboratedPlan'',
            elaboratedShared,
            pkgsBuildStatus,
            buildSettings)

type PlanDetails = (DistDirLayout,
                    ElaboratedInstallPlan,
                    ElaboratedSharedConfig,
                    BuildStatusMap,
                    BuildTimeSettings)

executePlan :: PlanDetails -> IO ElaboratedInstallPlan
executePlan (distDirLayout,
             elaboratedPlan,
             elaboratedShared,
             pkgsBuildStatus,
             buildSettings) =
    rebuildTargets verbosity
                   distDirLayout
                   elaboratedPlan
                   elaboratedShared
                   pkgsBuildStatus
                   -- Avoid trying to use act-as-setup mode:
                   buildSettings { buildSettingNumJobs = 1 }

cleanProject :: FilePath -> IO ()
cleanProject testdir = do
    alreadyExists <- doesDirectoryExist distDir
    when alreadyExists $ removeDirectoryRecursive distDir
  where
    projectRootDir = "tests" </> "IntegrationTests2" </> testdir
    distDirLayout  = defaultDistDirLayout projectRootDir
    distDir        = distDirectory distDirLayout


verbosity :: Verbosity
verbosity = minBound --normal --verbose --maxBound --minBound

---------------------------------------
-- HUint style utils for this context
--

expectException :: Exception e => String -> IO a -> IO e
expectException expected action = do
    res <- try action
    case res of
      Left  e -> return e
      Right _ -> throwIO $ HUnitFailure $ "expected an exception " ++ expected

expectPackagePreExisting :: ElaboratedInstallPlan -> PackageId
                         -> IO InstalledPackageInfo
expectPackagePreExisting plan pkgid = do
    planpkg <- expectPlanPackage plan pkgid
    case planpkg of
      InstallPlan.PreExisting pkg
        -> return pkg
      _ -> unexpectedPackageState "PreExisting" planpkg

expectPackageConfigured :: ElaboratedInstallPlan -> PackageId
                        -> IO ElaboratedConfiguredPackage
expectPackageConfigured plan pkgid = do
    planpkg <- expectPlanPackage plan pkgid
    case planpkg of
      InstallPlan.Configured pkg
        -> return pkg
      _ -> unexpectedPackageState "Configured" planpkg

expectPackageInstalled :: ElaboratedInstallPlan -> PackageId
                       -> IO (ElaboratedConfiguredPackage,
                              Maybe InstalledPackageInfo,
                              BuildSuccess)
expectPackageInstalled plan pkgid = do
    planpkg <- expectPlanPackage plan pkgid
    case planpkg of
      InstallPlan.Installed (ReadyPackage pkg) mipkg result
        -> return (pkg, mipkg, result)
      _ -> unexpectedPackageState "Installed" planpkg

expectPackageFailed :: ElaboratedInstallPlan -> PackageId
                    -> IO (ElaboratedConfiguredPackage,
                           BuildFailure)
expectPackageFailed plan pkgid = do
    planpkg <- expectPlanPackage plan pkgid
    case planpkg of
      InstallPlan.Failed pkg failure
        -> return (pkg, failure)
      _ -> unexpectedPackageState "Failed" planpkg

unexpectedPackageState :: String -> ElaboratedPlanPackage -> IO a
unexpectedPackageState expected planpkg =
    throwIO $ HUnitFailure $
         "expected to find " ++ display (packageId planpkg) ++ " in the "
      ++ expected ++ " state, but it is actually in the " ++ actual ++ "state."
  where
    actual = case planpkg of
      InstallPlan.PreExisting{} -> "PreExisting"
      InstallPlan.Configured{}  -> "Configured"
      InstallPlan.Processing{}  -> "Processing"
      InstallPlan.Installed{}   -> "Installed"
      InstallPlan.Failed{}      -> "Failed"

expectPlanPackage :: ElaboratedInstallPlan -> PackageId
                  -> IO ElaboratedPlanPackage
expectPlanPackage plan pkgid =
    case [ pkg
         | pkg <- InstallPlan.toList plan
         , packageId pkg == pkgid ] of
      [pkg] -> return pkg
      []    -> throwIO $ HUnitFailure $
                   "expected to find " ++ display pkgid
                ++ " in the install plan but it's not there"
      _     -> throwIO $ HUnitFailure $
                   "expected to find only one instance of " ++ display pkgid
                ++ " in the install plan but there's several"

