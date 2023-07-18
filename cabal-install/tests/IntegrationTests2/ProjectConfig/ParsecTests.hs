{-# LANGUAGE RecordWildCards #-}

-- | Integration Tests related to parsing of ProjectConfigs
module IntegrationTests2.ProjectConfig.ParsecTests (parserTests) where

import qualified Data.ByteString as BS
import Data.Either
import Distribution.Client.BuildReports.Types
import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Parsec
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.Client.Types.SourceRepo
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs (toPathTemplate)
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.PackageName
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Types.SourceRepo (KnownRepoType (..), RepoType (..))
import Distribution.Types.Version (mkVersion)
import Distribution.Types.VersionRange.Internal (VersionRange (..))
import Distribution.Utils.NubList
import Distribution.Verbosity
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

-- TODO create tests:
-- - parser tests to read and compare to expected values
-- - golden tests for warnings and errors
parserTests :: [TestTree]
parserTests =
  [ testCase "read packages" testPackages
  , testCase "read optional-packages" testOptionalPackages
  , testCase "read extra-packages" testExtraPackages
  , testCase "read source-repository-package" testSourceRepoList
  , testCase "read project-config-build-only" testProjectConfigBuildOnly
  ]

testPackages :: Assertion
testPackages = do
  let expected = [".", "packages/packages.cabal"]
  -- Note that I currently also run the legacy parser to make sure my expected values
  -- do not differ from the non-Parsec implementation, this will be removed in the future
  (config, legacy) <- readConfigDefault "packages"
  assertConfig expected config legacy (projectPackages . condTreeData)

testOptionalPackages :: Assertion
testOptionalPackages = do
  let expected = [".", "packages/packages.cabal"]
  (config, legacy) <- readConfigDefault "optional-packages"
  assertConfig expected config legacy (projectPackagesOptional . condTreeData)

testSourceRepoList :: Assertion
testSourceRepoList = do
  let expected =
        [ SourceRepositoryPackage
            { srpType = KnownRepoType Git
            , srpLocation = "https://example.com/Project.git"
            , srpTag = Just "1234"
            , srpBranch = Nothing
            , srpSubdir = []
            , srpCommand = []
            }
        , SourceRepositoryPackage
            { srpType = KnownRepoType Git
            , srpLocation = "https://example.com/example-dir/"
            , srpTag = Just "12345"
            , srpBranch = Nothing
            , srpSubdir = ["subproject"]
            , srpCommand = []
            }
        ]
  (config, legacy) <- readConfigDefault "source-repository-packages"
  assertConfig expected config legacy (projectPackagesRepo . condTreeData)

testExtraPackages :: Assertion
testExtraPackages = do
  let expected =
        [ PackageVersionConstraint (mkPackageName "a") (OrLaterVersion (mkVersion [0]))
        , PackageVersionConstraint (mkPackageName "b") (IntersectVersionRanges (OrLaterVersion (mkVersion [0, 7, 3])) (EarlierVersion (mkVersion [0, 9])))
        ]
  (config, legacy) <- readConfigDefault "extra-packages"
  assertConfig expected config legacy (projectPackagesNamed . condTreeData)

testProjectConfigBuildOnly :: Assertion
testProjectConfigBuildOnly = do
  let expected = ProjectConfigBuildOnly{..}
  (config, legacy) <- readConfigDefault "project-config-build-only"
  assertConfig expected config legacy (projectConfigBuildOnly . condTreeData)
  where
    projectConfigVerbosity = toFlag (toEnum 2)
    projectConfigDryRun = mempty -- cli only
    projectConfigOnlyDeps = mempty -- cli only
    projectConfigOnlyDownload = mempty -- cli only
    projectConfigSummaryFile = toNubList [toPathTemplate "summaryFile"]
    projectConfigLogFile = toFlag $ toPathTemplate "myLog.log"
    projectConfigBuildReports = mempty -- cli only
    projectConfigReportPlanningFailure = toFlag True
    projectConfigSymlinkBinDir = toFlag "some-bindir"
    projectConfigNumJobs = toFlag $ Just 4
    projectConfigKeepGoing = toFlag True
    projectConfigOfflineMode = toFlag True
    projectConfigKeepTempFiles = toFlag True
    projectConfigHttpTransport = toFlag "wget"
    projectConfigIgnoreExpiry = toFlag True
    projectConfigCacheDir = toFlag "some-cache-dir"
    projectConfigLogsDir = toFlag "logs-directory"
    projectConfigClientInstallFlags = mempty -- cli only

readConfigDefault :: FilePath -> IO (ProjectConfigSkeleton, ProjectConfigSkeleton)
readConfigDefault rootFp = readConfig rootFp "cabal.project"

readConfig :: FilePath -> FilePath -> IO (ProjectConfigSkeleton, ProjectConfigSkeleton)
readConfig rootFp projectFileName = do
  projectRootDir <- canonicalizePath (basedir </> rootFp)

  let projectRoot = ProjectRootExplicit projectRootDir projectFileName
      extensionName = ""
      distDirLayout = defaultDistDirLayout projectRoot Nothing Nothing
      extensionDescription = "description"
      distProjectConfigFp = distProjectFile distDirLayout extensionName
  exists <- doesFileExist distProjectConfigFp
  assertBool ("projectConfig does not exist: " <> distProjectConfigFp) exists
  contents <- BS.readFile distProjectConfigFp
  let (_, res) = runParseResult $ parseProjectSkeleton contents
  assertBool ("should parse successfully: " ++ show res) $ isRight res
  let parsec = fromRight undefined res
  httpTransport <- configureTransport verbosity [] Nothing
  legacy <-
    runRebuild projectRootDir $
      readProjectFileSkeletonLegacy verbosity httpTransport distDirLayout extensionName extensionDescription
  return (parsec, legacy)

assertConfig' :: (Eq a, Show a) => a -> ProjectConfigSkeleton -> (ProjectConfigSkeleton -> a) -> IO ()
assertConfig' expected config access = expected @=? actual
  where
    actual = access config

assertConfig :: (Eq a, Show a) => a -> ProjectConfigSkeleton -> ProjectConfigSkeleton -> (ProjectConfigSkeleton -> a) -> IO ()
assertConfig expected config configLegacy access = do
  expected @=? actualLegacy
  expected @=? actual
  where
    actual = access config
    actualLegacy = access configLegacy

-- | Test Utilities
verbosity :: Verbosity
verbosity = normal -- minBound --normal --verbose --maxBound --minBound

basedir :: FilePath
basedir = "tests" </> "IntegrationTests2" </> "ProjectConfig" </> "files"
