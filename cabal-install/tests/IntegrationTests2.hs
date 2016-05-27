{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Main where

import Distribution.Client.DistDirLayout
import Distribution.Client.ProjectConfig
import Distribution.Client.Config (defaultCabalDir)
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.ProjectBuilding
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.Types (GenericReadyPackage(..), installedPackageId)

import Distribution.Package hiding (installedPackageId)
import Distribution.PackageDescription
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Simple.Setup (toFlag)
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
import Test.Tasty.Options
import Data.Tagged (Tagged(..))
import Data.Proxy  (Proxy(..))
import Data.Typeable (Typeable)


main :: IO ()
main =
  defaultMainWithIngredients
    (defaultIngredients ++ [includingOptions projectConfigOptionDescriptions])
    (withProjectConfig $ \config ->
      testGroup "Integration tests (internal)"
                (tests config))

tests :: ProjectConfig -> [TestTree]
tests config =
    --TODO: tests for:
    -- * normal success
    -- * dry-run tests with changes
  [ testGroup "Exceptions during discovey and planning" $
    [ testCase "no package"  (testExceptionInFindingPackage config)
    , testCase "no package2" (testExceptionInFindingPackage2 config)
    ]
  , testGroup "Exceptions during building (local inplace)" $
    [ testCase "configure"   (testExceptionInConfigureStep config)
    , testCase "build"       (testExceptionInBuildStep config)
--    , testCase "register"   testExceptionInRegisterStep
    ]
    --TODO: need to repeat for packages for the store

  , testGroup "Successful builds" $
    [ testCaseSteps "Setup script styles" (testSetupScriptStyles config)
    ]

  , testGroup "Regression tests" $
    [ testCase "issue #3324" (testRegressionIssue3324 config)
    ]
  ]

testExceptionInFindingPackage :: ProjectConfig -> Assertion
testExceptionInFindingPackage config = do
    BadPackageLocations locs <- expectException "BadPackageLocations" $
      void $ planProject testdir config
    case locs of
      [BadLocGlobEmptyMatch "./*.cabal"] -> return ()
      _ -> assertFailure "expected BadLocGlobEmptyMatch"
    cleanProject testdir
  where
    testdir = "exception/no-pkg"


testExceptionInFindingPackage2 :: ProjectConfig -> Assertion
testExceptionInFindingPackage2 config = do
    BadPackageLocations locs <- expectException "BadPackageLocations" $
      void $ planProject testdir config
    case locs of
      [BadLocGlobBadMatches "./" [BadLocDirNoCabalFile "."]] -> return ()
      _ -> assertFailure $ "expected BadLocGlobBadMatches, got " ++ show locs
    cleanProject testdir
  where
    testdir = "exception/no-pkg2"


testExceptionInConfigureStep :: ProjectConfig -> Assertion
testExceptionInConfigureStep config = do
    plan  <- planProject testdir config
    plan' <- executePlan plan
    (_pkga1, failure) <- expectPackageFailed plan' pkgidA1
    case failure of
      ConfigureFailed _str -> return ()
      _ -> assertFailure $ "expected ConfigureFailed, got " ++ show failure 
    cleanProject testdir
  where
    testdir = "exception/configure"
    pkgidA1 = PackageIdentifier (PackageName "a") (Version [1] [])


testExceptionInBuildStep :: ProjectConfig -> Assertion
testExceptionInBuildStep config = do
    plan  <- planProject testdir config
    plan' <- executePlan plan
    (_pkga1, failure) <- expectPackageFailed plan' pkgidA1
    expectBuildFailed failure
  where
    testdir = "exception/build"
    pkgidA1 = PackageIdentifier (PackageName "a") (Version [1] [])

testSetupScriptStyles :: ProjectConfig -> (String -> IO ()) -> Assertion
testSetupScriptStyles config reportSubCase = do

    reportSubCase (show SetupCustomExplicitDeps)
    plan1  <- executePlan =<< planProject testdir1 config
    (pkg1, _, _) <- expectPackageInstalled plan1 pkgidA
    pkgSetupScriptStyle pkg1 @?= SetupCustomExplicitDeps
    hasDefaultSetupDeps pkg1 @?= Just False
    marker1 <- readFile (basedir </> testdir1 </> "marker")
    marker1 @?= "ok"
    removeFile (basedir </> testdir1 </> "marker")

    reportSubCase (show SetupCustomImplicitDeps)
    plan2  <- executePlan =<< planProject testdir2 config
    (pkg2, _, _) <- expectPackageInstalled plan2 pkgidA
    pkgSetupScriptStyle pkg2 @?= SetupCustomImplicitDeps
    hasDefaultSetupDeps pkg2 @?= Just True
    marker2 <- readFile (basedir </> testdir2 </> "marker")
    marker2 @?= "ok"
    removeFile (basedir </> testdir2 </> "marker")

    reportSubCase (show SetupNonCustomInternalLib)
    plan3  <- executePlan =<< planProject testdir3 config
    (pkg3, _, _) <- expectPackageInstalled plan3 pkgidA
    pkgSetupScriptStyle pkg3 @?= SetupNonCustomInternalLib
{-
    --TODO: the SetupNonCustomExternalLib case is hard to test since it
    -- requires a version of Cabal that's later than the one we're testing
    -- e.g. needs a .cabal file that specifies cabal-version: >= 2.0
    -- and a corresponding Cabal package that we can use to try and build a
    -- default Setup.hs.
    reportSubCase (show SetupNonCustomExternalLib)
    plan4  <- executePlan =<< planProject testdir4 config
    (pkg4, _, _) <- expectPackageInstalled plan4 pkgidA
    pkgSetupScriptStyle pkg4 @?= SetupNonCustomExternalLib
-}
  where
    testdir1 = "build/setup-custom1"
    testdir2 = "build/setup-custom2"
    testdir3 = "build/setup-simple"
    pkgidA   = PackageIdentifier (PackageName "a") (Version [0,1] [])
    -- The solver fills in default setup deps explicitly, but marks them as such
    hasDefaultSetupDeps = fmap defaultSetupDepends
                        . setupBuildInfo . pkgDescription

-- | See <https://github.com/haskell/cabal/issues/3324>
--
testRegressionIssue3324 :: ProjectConfig -> Assertion
testRegressionIssue3324 config = do
    -- expected failure first time due to missing dep
    plan1 <- executePlan =<< planProject testdir config
    (_pkgq, failure) <- expectPackageFailed plan1 pkgidQ
    expectBuildFailed failure

    -- add the missing dep, now it should work
    let qcabal  = basedir </> testdir </> "q" </> "q.cabal"
    withFileFinallyRestore qcabal $ do
      appendFile qcabal ("  build-depends: p\n")
      plan2 <- executePlan =<< planProject testdir config
      _ <- expectPackageInstalled plan2 pkgidP
      _ <- expectPackageInstalled plan2 pkgidQ
      return ()
  where
    testdir = "regression/3324"
    pkgidP  = PackageIdentifier (PackageName "p") (Version [0,1] [])
    pkgidQ  = PackageIdentifier (PackageName "q") (Version [0,1] [])


---------------------------------
-- Test utils to plan and build
--

basedir :: FilePath
basedir = "tests" </> "IntegrationTests2"

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



-------------------------------------------
-- Tasty integration to adjust the config
--

withProjectConfig :: (ProjectConfig -> TestTree) -> TestTree
withProjectConfig testtree =
    askOption $ \ghcPath ->
      testtree (mkProjectConfig ghcPath)

mkProjectConfig :: GhcPath -> ProjectConfig
mkProjectConfig (GhcPath ghcPath) =
    mempty {
      projectConfigShared = mempty {
        projectConfigHcPath = maybeToFlag ghcPath
      },
     projectConfigBuildOnly = mempty {
       projectConfigNumJobs = toFlag (Just 1)
     }
    }
  where
    maybeToFlag = maybe mempty toFlag


data GhcPath = GhcPath (Maybe FilePath)
  deriving Typeable

instance IsOption GhcPath where
  defaultValue = GhcPath Nothing
  optionName   = Tagged "with-ghc"
  optionHelp   = Tagged "The ghc compiler to use"
  parseValue   = Just . GhcPath . Just

projectConfigOptionDescriptions :: [OptionDescription]
projectConfigOptionDescriptions = [Option (Proxy :: Proxy GhcPath)]


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

expectBuildFailed :: BuildFailure -> IO ()
expectBuildFailed (BuildFailed _str) = return ()
expectBuildFailed failure = assertFailure $ "expected BuildFailed, got "
                                         ++ show failure

---------------------------------------
-- Other utils
--

-- | Allow altering a file during a test, but then restore it afterwards
--
withFileFinallyRestore :: FilePath -> IO a -> IO a
withFileFinallyRestore file action = do
    copyFile file backup
    action `finally` renameFile backup file
  where
    backup = file <.> "backup"
