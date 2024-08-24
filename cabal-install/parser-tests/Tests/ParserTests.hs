{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tests for the project file parser
module Tests.ParserTests (parserTests) where

import Control.Monad.IO.Class
  ( MonadIO (liftIO)
  )
import Data.Either (fromRight)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Distribution.Client.BuildReports.Types (ReportLevel (..))
import Distribution.Client.CmdInstall.ClientInstallFlags (ClientInstallFlags (..))
import Distribution.Client.Dependency.Types (PreSolver (..))
import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.IndexUtils.ActiveRepos (ActiveRepoEntry (..), ActiveRepos (..), CombineStrategy (..))
import Distribution.Client.IndexUtils.IndexState (RepoIndexState (..), headTotalIndexState, insertIndexState)
import Distribution.Client.ProjectConfig
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.Client.Targets (readUserConstraint)
import Distribution.Client.Types.AllowNewer (AllowNewer (..), AllowOlder (..), RelaxDepMod (..), RelaxDepScope (..), RelaxDepSubject (..), RelaxDeps (..), RelaxedDep (..))
import Distribution.Client.Types.InstallMethod (InstallMethod (..))
import Distribution.Client.Types.OverwritePolicy (OverwritePolicy (..))
import Distribution.Client.Types.Repo (LocalRepo (..), RemoteRepo (..))
import Distribution.Client.Types.RepoName (RepoName (..))
import Distribution.Client.Types.SourceRepo
import Distribution.Client.Types.WriteGhcEnvironmentFilesPolicy (WriteGhcEnvironmentFilesPolicy (..))
import Distribution.Compat.Prelude
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Parsec (simpleParsec)
import Distribution.Simple.Compiler (DebugInfoLevel (..), OptimisationLevel (..), PackageDB (..), ProfDetailLevel (..))
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs (InstallDirs (..), toPathTemplate)
import Distribution.Simple.Setup (DumpBuildInfo (..), HaddockTarget (..), TestShowDetails (..))
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))
import Distribution.Solver.Types.ProjectConfigPath (ProjectConfigPath (..))
import Distribution.Solver.Types.Settings
  ( AllowBootLibInstalls (..)
  , CountConflicts (..)
  , FineGrainedConflicts (..)
  , IndependentGoals (..)
  , MinimizeConflictSet (..)
  , OnlyConstrained (..)
  , PreferOldest (..)
  , ReorderGoals (..)
  , StrongFlags (..)
  )
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.Flag (mkFlagAssignment)
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.PackageName
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Types.SourceRepo (KnownRepoType (..), RepoType (..))
import Distribution.Types.Version (mkVersion)
import Distribution.Types.VersionRange.Internal (VersionRange (..))
import Distribution.Utils.NubList
import Distribution.Verbosity
import Network.URI (parseURI)
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath ((</>))
import Prelude ()

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

parserTests :: TestTree
parserTests =
  testGroup
    "project files parsec tests"
    [ testCase "read packages" testPackages
    , testCase "read optional-packages" testOptionalPackages
    , testCase "read extra-packages" testExtraPackages
    , testCase "read source-repository-package" testSourceRepoList
    , testCase "read project-config-build-only" testProjectConfigBuildOnly
    , testCase "read project-config-shared" testProjectConfigShared
    , testCase "read install-dirs" testInstallDirs
    , testCase "read remote-repos" testRemoteRepos
    , testCase "read local-no-index-repos" testLocalNoIndexRepos
    , testCase "set explicit provenance" testProjectConfigProvenance
    , testCase "read project-config-local-packages" testProjectConfigLocalPackages
    , testCase "read project-config-all-packages" testProjectConfigAllPackages
    , testCase "read project-config-specific-packages" testProjectConfigSpecificPackages
    , testCase "test projectConfigAllPackages concatenation" testAllPackagesConcat
    , testCase "test projectConfigSpecificPackages concatenation" testSpecificPackagesConcat
    , testCase "test program-locations concatenation" testProgramLocationsConcat
    , testCase "test program-options concatenation" testProgramOptionsConcat
    , testCase "test allow-newer and allow-older concatenation" testRelaxDepsConcat
    ]

testPackages :: Assertion
testPackages = do
  let expected = [".", "packages/packages.cabal"]
  (config, legacy) <- readConfigDefault "packages"
  assertConfigEquals expected config legacy (projectPackages . condTreeData)

testOptionalPackages :: Assertion
testOptionalPackages = do
  let expected = [".", "packages/packages.cabal"]
  (config, legacy) <- readConfigDefault "optional-packages"
  assertConfigEquals expected config legacy (projectPackagesOptional . condTreeData)

testSourceRepoList :: Assertion
testSourceRepoList = do
  (config, legacy) <- readConfigDefault "source-repository-packages"
  assertConfigEquals expected config legacy (projectPackagesRepo . condTreeData)
  where
    expected =
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

testExtraPackages :: Assertion
testExtraPackages = do
  (config, legacy) <- readConfigDefault "extra-packages"
  assertConfigEquals expected config legacy (projectPackagesNamed . condTreeData)
  where
    expected =
      [ PackageVersionConstraint (mkPackageName "a") (OrLaterVersion (mkVersion [0]))
      , PackageVersionConstraint (mkPackageName "b") (IntersectVersionRanges (OrLaterVersion (mkVersion [0, 7, 3])) (EarlierVersion (mkVersion [0, 9])))
      ]

testProjectConfigBuildOnly :: Assertion
testProjectConfigBuildOnly = do
  (config, legacy) <- readConfigDefault "project-config-build-only"
  assertConfigEquals expected config legacy (projectConfigBuildOnly . condTreeData)
  where
    expected = ProjectConfigBuildOnly{..}
    projectConfigVerbosity = toFlag (toEnum 2)
    projectConfigDryRun = mempty -- cli only
    projectConfigOnlyDeps = mempty -- cli only
    projectConfigOnlyDownload = mempty -- cli only
    projectConfigSummaryFile = toNubList [toPathTemplate "summaryFile", toPathTemplate "summaryFile2"]
    projectConfigLogFile = toFlag $ toPathTemplate "myLog.log"
    projectConfigBuildReports = toFlag $ DetailedReports
    projectConfigReportPlanningFailure = toFlag True
    projectConfigSymlinkBinDir = toFlag "some-bindir"
    projectConfigNumJobs = toFlag $ Just 4
    projectConfigUseSemaphore = toFlag True
    projectConfigKeepGoing = toFlag True
    projectConfigOfflineMode = toFlag True
    projectConfigKeepTempFiles = toFlag True
    projectConfigHttpTransport = toFlag "wget"
    projectConfigIgnoreExpiry = toFlag True
    projectConfigCacheDir = toFlag "some-cache-dir"
    projectConfigLogsDir = toFlag "logs-directory"
    projectConfigClientInstallFlags =
      ClientInstallFlags
        { cinstInstallLibs = Flag True
        , cinstEnvironmentPath = Flag "path/to/env"
        , cinstOverwritePolicy = Flag AlwaysOverwrite
        , cinstInstallMethod = Flag InstallMethodSymlink
        , cinstInstalldir = Flag "path/to/installdir"
        }

testProjectConfigShared :: Assertion
testProjectConfigShared = do
  (config, legacy) <- readConfigDefault "project-config-shared"
  assertConfigEquals expected config legacy (projectConfigShared . condTreeData)
  where
    expected = ProjectConfigShared{..}
    projectConfigDistDir = toFlag "something"
    projectConfigConfigFile = mempty -- cli only
    projectConfigProjectDir = toFlag "my-project-dir"
    projectConfigProjectFile = toFlag "my-project"
    projectConfigIgnoreProject = toFlag False
    projectConfigHcFlavor = toFlag GHCJS
    projectConfigHcPath = toFlag "/some/path/to/compiler"
    projectConfigHcPkg = toFlag "/some/path/to/ghc-pkg"
    projectConfigHaddockIndex = toFlag $ toPathTemplate "/path/to/haddock-index"
    projectConfigInstallDirs = mempty -- tested below in testInstallDirs
    projectConfigPackageDBs = [Nothing, Just (SpecificPackageDB "foo"), Nothing, Just (SpecificPackageDB "bar"), Just (SpecificPackageDB "baz")]
    projectConfigRemoteRepos = mempty -- tested below in testRemoteRepos
    projectConfigLocalNoIndexRepos = mempty -- tested below in testLocalNoIndexRepos
    projectConfigActiveRepos = Flag (ActiveRepos [ActiveRepo (RepoName "hackage.haskell.org") CombineStrategyMerge, ActiveRepo (RepoName "my-repository") CombineStrategyOverride])
    projectConfigIndexState =
      let
        hackageState = IndexStateTime $ fromJust $ simpleParsec "2020-05-06T22:33:27Z"
        indexState' = insertIndexState (RepoName "hackage.haskell.org") hackageState headTotalIndexState
        headHackageState = IndexStateTime $ fromJust $ simpleParsec "2020-04-29T04:11:05Z"
        indexState'' = insertIndexState (RepoName "head.hackage") headHackageState indexState'
       in
        toFlag indexState''
    projectConfigStoreDir = toFlag "a/store/dir/path" -- cli only
    projectConfigConstraints =
      let
        bar = fromRight (error "error parsing bar") $ readUserConstraint "bar == 2.1"
        barFlags = fromRight (error "error parsing bar flags") $ readUserConstraint "bar +foo -baz"
        source = ConstraintSourceProjectConfig $ ProjectConfigPath $ "cabal.project" :| []
       in
        [(bar, source), (barFlags, source)]
    projectConfigPreferences = [PackageVersionConstraint (mkPackageName "foo") (ThisVersion (mkVersion [0, 9])), PackageVersionConstraint (mkPackageName "baz") (LaterVersion (mkVersion [2, 0]))]
    projectConfigCabalVersion = Flag (mkVersion [1, 24, 0, 1])
    projectConfigSolver = Flag AlwaysModular
    projectConfigAllowOlder = Just (AllowOlder $ RelaxDepsSome [RelaxedDep RelaxDepScopeAll RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "dep")), RelaxedDep (RelaxDepScopePackageId (PackageIdentifier (mkPackageName "pkga") (mkVersion [1, 1, 2]))) RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "dep-pkg"))])
    projectConfigAllowNewer = Just (AllowNewer $ RelaxDepsSome [RelaxedDep (RelaxDepScopePackageId (PackageIdentifier (mkPackageName "pkgb") (mkVersion [1, 2, 3]))) RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "dep-pkgb")), RelaxedDep RelaxDepScopeAll RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "importantlib"))])
    projectConfigWriteGhcEnvironmentFilesPolicy = Flag AlwaysWriteGhcEnvironmentFiles
    projectConfigMaxBackjumps = toFlag 42
    projectConfigReorderGoals = Flag (ReorderGoals True)
    projectConfigCountConflicts = Flag (CountConflicts False)
    projectConfigFineGrainedConflicts = Flag (FineGrainedConflicts False)
    projectConfigMinimizeConflictSet = Flag (MinimizeConflictSet True)
    projectConfigStrongFlags = Flag (StrongFlags True)
    projectConfigAllowBootLibInstalls = Flag (AllowBootLibInstalls True)
    projectConfigOnlyConstrained = Flag OnlyConstrainedAll
    projectConfigPerComponent = Flag True
    projectConfigIndependentGoals = Flag (IndependentGoals True)
    projectConfigPreferOldest = Flag (PreferOldest True)
    projectConfigProgPathExtra = toNubList ["/foo/bar", "/baz/quux"]
    projectConfigMultiRepl = toFlag True

testInstallDirs :: Assertion
testInstallDirs = do
  (config, legacy) <- readConfigDefault "install-dirs"
  assertConfigEquals expected config legacy (projectConfigInstallDirs . projectConfigShared . condTreeData)
  where
    expected =
      InstallDirs
        { prefix = Flag $ toPathTemplate "my/prefix-path"
        , bindir = Flag $ toPathTemplate "bin/dir/"
        , libdir = Flag $ toPathTemplate "lib/dir/path"
        , libsubdir = Flag $ toPathTemplate "/lib/sub/dir"
        , dynlibdir = Flag $ toPathTemplate "dyn/lib/dir/path"
        , flibdir = mempty
        , libexecdir = Flag $ toPathTemplate "lib/exec/dir/"
        , libexecsubdir = Flag $ toPathTemplate "libexec/subdir"
        , includedir = mempty
        , datadir = Flag $ toPathTemplate "path/to/datadir/"
        , datasubdir = Flag $ toPathTemplate "a/datadir/subdir"
        , docdir = Flag $ toPathTemplate "path/to/docs"
        , mandir = mempty
        , htmldir = Flag $ toPathTemplate "dir/html/"
        , haddockdir = Flag $ toPathTemplate "haddock/dir"
        , sysconfdir = Flag $ toPathTemplate "sys/conf/dir"
        }

testRemoteRepos :: Assertion
testRemoteRepos = do
  (config, legacy) <- readConfigDefault "remote-repos"
  let actualRemoteRepos = (fromNubList . projectConfigRemoteRepos . projectConfigShared . condTreeData) config
  assertBool "Expected RemoteRepos do not match parsed values" $ compareLists expected actualRemoteRepos compareRemoteRepos
  assertConfigEquals mempty config legacy (projectConfigLocalNoIndexRepos . projectConfigShared . condTreeData)
  where
    expected = [packagesRepository, morePackagesRepository, secureLocalRepository]
    packagesRepository =
      RemoteRepo
        { remoteRepoName = RepoName $ "packages.example.org"
        , remoteRepoURI = fromJust $ parseURI "http://packages.example.org/"
        , remoteRepoSecure = pure True
        , remoteRepoRootKeys = ["21", "42"]
        , remoteRepoKeyThreshold = 2
        , remoteRepoShouldTryHttps = False
        }
    morePackagesRepository =
      RemoteRepo
        { remoteRepoName = RepoName $ "more-packages.example.org"
        , remoteRepoURI = fromJust $ parseURI "https://more-packages.example.org/"
        , remoteRepoSecure = pure True
        , remoteRepoRootKeys = ["foo", "bar"]
        , remoteRepoKeyThreshold = 1
        , remoteRepoShouldTryHttps = False
        }
    secureLocalRepository =
      RemoteRepo
        { remoteRepoName = RepoName $ "my-secure-local-repository"
        , remoteRepoURI = fromJust $ parseURI "file:/path/to/secure/repo"
        , remoteRepoSecure = pure True
        , remoteRepoRootKeys = ["123"]
        , remoteRepoKeyThreshold = 1
        , remoteRepoShouldTryHttps = False
        }

testLocalNoIndexRepos :: Assertion
testLocalNoIndexRepos = do
  (config, legacy) <- readConfigDefault "local-no-index-repos"
  let actualLocalRepos = (fromNubList . projectConfigLocalNoIndexRepos . projectConfigShared . condTreeData) config
  assertBool "Expected LocalNoIndexRepos do not match parsed values" $ compareLists expected actualLocalRepos compareLocalRepos
  assertConfigEquals mempty config legacy (projectConfigRemoteRepos . projectConfigShared . condTreeData)
  where
    expected = [myRepository, mySecureRepository]
    myRepository =
      LocalRepo
        { localRepoName = RepoName $ "my-repository"
        , localRepoPath = "/absolute/path/to/directory"
        , localRepoSharedCache = False
        }
    mySecureRepository =
      LocalRepo
        { localRepoName = RepoName $ "my-other-repository"
        , localRepoPath = "/another/path/to/repository"
        , localRepoSharedCache = False
        }

testProjectConfigProvenance :: Assertion
testProjectConfigProvenance = do
  let expected = Set.singleton (Explicit (ProjectConfigPath $ "cabal.project" :| []))
  (config, legacy) <- readConfigDefault "empty"
  assertConfigEquals expected config legacy (projectConfigProvenance . condTreeData)

testProjectConfigLocalPackages :: Assertion
testProjectConfigLocalPackages = do
  (config, legacy) <- readConfigDefault "project-config-local-packages"
  assertConfigEquals expected config legacy (projectConfigLocalPackages . condTreeData)
  where
    expected = PackageConfig{..}
    packageConfigProgramPaths = MapLast $ Map.fromList [("ghc", "/tmp/bin/ghc"), ("gcc", "/tmp/bin/gcc")]
    packageConfigProgramArgs = MapMappend $ Map.fromList [("ghc", ["-fno-state-hack", "-foo"]), ("gcc", ["-baz", "-quux"])]
    packageConfigProgramPathExtra = toNubList ["/tmp/bin/extra", "/usr/local/bin"]
    packageConfigFlagAssignment = mkFlagAssignment [("foo", True), ("bar", False)]
    packageConfigVanillaLib = Flag False
    packageConfigSharedLib = Flag True
    packageConfigStaticLib = Flag True
    packageConfigDynExe = Flag True
    packageConfigFullyStaticExe = Flag True
    packageConfigProf = Flag True
    packageConfigProfLib = Flag True
    packageConfigProfShared = Flag False
    packageConfigProfExe = Flag True
    packageConfigProfDetail = Flag ProfDetailAllFunctions
    packageConfigProfLibDetail = Flag ProfDetailExportedFunctions
    packageConfigConfigureArgs = ["-some-arg", "/some/path"]
    packageConfigOptimization = Flag MaximumOptimisation
    packageConfigProgPrefix = Flag $ toPathTemplate "another/path"
    packageConfigProgSuffix = Flag $ toPathTemplate "and/another/path"
    packageConfigExtraLibDirs = ["so", "many", "lib/dirs"]
    packageConfigExtraLibDirsStatic = ["a/few", "static/lib/dirs"]
    packageConfigExtraFrameworkDirs = ["osx/framework", "dirs"]
    packageConfigExtraIncludeDirs = ["incredible/amount", "of", "include", "directories"]
    packageConfigGHCiLib = Flag False
    packageConfigSplitSections = Flag True
    packageConfigSplitObjs = Flag True
    packageConfigStripExes = Flag False
    packageConfigStripLibs = Flag False
    packageConfigTests = Flag True
    packageConfigBenchmarks = Flag True
    packageConfigCoverage = Flag True
    packageConfigRelocatable = Flag True
    packageConfigDebugInfo = Flag MaximalDebugInfo
    packageConfigDumpBuildInfo = Flag DumpBuildInfo
    packageConfigRunTests = Flag True
    packageConfigDocumentation = Flag True
    -- Haddock options
    packageConfigHaddockHoogle = Flag True
    packageConfigHaddockHtml = Flag False
    packageConfigHaddockHtmlLocation = Flag "http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html"
    packageConfigHaddockForeignLibs = Flag True
    packageConfigHaddockExecutables = Flag True
    packageConfigHaddockTestSuites = Flag True
    packageConfigHaddockBenchmarks = Flag True
    packageConfigHaddockInternal = Flag True
    packageConfigHaddockCss = Flag "some/path/to/file.css"
    packageConfigHaddockLinkedSource = Flag True
    packageConfigHaddockQuickJump = Flag True
    packageConfigHaddockHscolourCss = Flag "another/path/to/hscolour.css"
    packageConfigHaddockContents = Flag $ toPathTemplate "https://example.com/$pkg/contents"
    packageConfigHaddockIndex = Flag $ toPathTemplate "separately-generated/HTML/index"
    packageConfigHaddockBaseUrl = Flag "https://example.com/haddock-base-url"
    packageConfigHaddockResourcesDir = Flag "/haddock/static"
    packageConfigHaddockOutputDir = Flag "/haddock/output"
    packageConfigHaddockUseUnicode = Flag False
    packageConfigHaddockForHackage = Flag ForHackage
    packageConfigTestHumanLog = Flag $ toPathTemplate "human-log.log"
    packageConfigTestMachineLog = Flag $ toPathTemplate "machine.log"
    packageConfigTestShowDetails = Flag Streaming
    packageConfigTestKeepTix = Flag True
    packageConfigTestWrapper = Flag "/test-wrapper-path/"
    packageConfigTestFailWhenNoTestSuites = Flag True
    packageConfigTestTestOptions = [toPathTemplate "--some-option", toPathTemplate "42"]
    packageConfigBenchmarkOptions = [toPathTemplate "--some-benchmark-option", toPathTemplate "--another-option"]

testProjectConfigAllPackages :: Assertion
testProjectConfigAllPackages = do
  (config, legacy) <- readConfigDefault "project-config-all-packages"
  assertConfigEquals expected config legacy (projectConfigAllPackages . condTreeData)
  where
    expected :: PackageConfig
    expected =
      mempty
        { packageConfigProfDetail = Flag ProfDetailAllFunctions
        , packageConfigProfLibDetail = Flag ProfDetailExportedFunctions
        }

testProjectConfigSpecificPackages :: Assertion
testProjectConfigSpecificPackages = do
  (config, legacy) <- readConfigDefault "project-config-specific-packages"
  assertConfigEquals expected config legacy (projectConfigSpecificPackage . condTreeData)
  where
    expected = MapMappend $ Map.fromList [("foo", expectedFoo), ("bar", expectedBar), ("baz", expectedBaz)]
    expectedFoo :: PackageConfig
    expectedFoo =
      mempty
        { packageConfigProfDetail = Flag ProfDetailAllFunctions
        , packageConfigProfLibDetail = Flag ProfDetailExportedFunctions
        , packageConfigVanillaLib = Flag True
        }
    expectedBar :: PackageConfig
    expectedBar =
      mempty
        { packageConfigProfDetail = Flag ProfDetailTopLate
        , packageConfigProfLibDetail = Flag ProfDetailNone
        , packageConfigProgPrefix = Flag $ toPathTemplate "prefix/path"
        }
    expectedBaz :: PackageConfig
    expectedBaz =
      mempty
        { packageConfigSharedLib = Flag True
        }

testAllPackagesConcat :: Assertion
testAllPackagesConcat = do
  (config, legacy) <- readConfigDefault "all-packages-concat"
  assertConfigEquals expected config legacy (projectConfigAllPackages . condTreeData)
  where
    expected :: PackageConfig
    expected =
      mempty
        { packageConfigSharedLib = Flag True
        , packageConfigStaticLib = Flag True
        , packageConfigProgramArgs =
            MapMappend $
              Map.fromList
                [ ("ghc", ["-fwarn-tabs", "-optc-fno-builtin-malloc", "-Wall", "-optc-fno-builtin-realloc", "-fwrite-ide-info"])
                ]
        }

testSpecificPackagesConcat :: Assertion
testSpecificPackagesConcat = do
  (config, legacy) <- readConfigDefault "specific-packages-concat"
  assertConfigEquals expected config legacy (projectConfigSpecificPackage . condTreeData)
  where
    expected = MapMappend $ Map.fromList [("foo", expectedFoo)]
    expectedFoo :: PackageConfig
    expectedFoo =
      mempty
        { packageConfigSharedLib = Flag True
        , packageConfigStaticLib = Flag True
        , packageConfigProgramArgs = MapMappend $ Map.fromList [("ghc", ["-fno-state-hack", "-threaded"])]
        }

testProgramLocationsConcat :: Assertion
testProgramLocationsConcat = do
  (config, legacy) <- readConfigDefault "program-locations-concat"
  assertConfigEquals expected config legacy (projectConfigLocalPackages . condTreeData)
  where
    expected :: PackageConfig
    expected =
      mempty
        { packageConfigProgramPaths = MapLast $ Map.fromList [("gcc", "/tmp/bin/gcc"), ("ghc", "/tmp/bin/ghc")]
        }

testProgramOptionsConcat :: Assertion
testProgramOptionsConcat = do
  (config, legacy) <- readConfigDefault "program-options-concat"
  assertConfigEquals expected config legacy (projectConfigLocalPackages . condTreeData)
  where
    expected :: PackageConfig
    expected =
      mempty
        { packageConfigProgramArgs = MapMappend $ Map.fromList [("ghc", ["-threaded", "-Wall", "-fno-state-hack"]), ("gcc", ["-baz", "-foo", "-bar"])]
        }

testRelaxDepsConcat :: Assertion
testRelaxDepsConcat = do
  (config, legacy) <- readConfigDefault "relax-deps-concat"
  assertConfigEquals expectedAllowNewer config legacy (projectConfigAllowNewer . projectConfigShared . condTreeData)
  assertConfigEquals expectedAllowOlder config legacy (projectConfigAllowOlder . projectConfigShared . condTreeData)
  where
    expectedAllowNewer :: Maybe AllowNewer
    expectedAllowNewer =
      pure $
        AllowNewer $
          RelaxDepsSome
            [ RelaxedDep (RelaxDepScopePackageId (PackageIdentifier (mkPackageName "cassava") (mkVersion [0, 5, 2, 0]))) RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "base"))
            , RelaxedDep (RelaxDepScopePackageId (PackageIdentifier (mkPackageName "vector-th-unbox") (mkVersion [0, 2, 1, 7]))) RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "base"))
            , RelaxedDep (RelaxDepScopePackageId (PackageIdentifier (mkPackageName "vector-th-unbox") (mkVersion [0, 2, 1, 7]))) RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "template-haskell"))
            ]
    expectedAllowOlder :: Maybe AllowOlder
    expectedAllowOlder =
      pure $
        AllowOlder $
          RelaxDepsSome
            [ RelaxedDep (RelaxDepScopePackageId (PackageIdentifier (mkPackageName "mtl") (mkVersion [2, 3, 1]))) RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "base"))
            , RelaxedDep (RelaxDepScopePackageId (PackageIdentifier (mkPackageName "aeson") (mkVersion [2, 2, 3, 0]))) RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "bytestring"))
            , RelaxedDep (RelaxDepScopePackageId (PackageIdentifier (mkPackageName "containers") (mkVersion [0, 7]))) RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "array"))
            ]

-------------------------------------------------------------------------------
-- Test Utilities
-------------------------------------------------------------------------------
baseDir :: FilePath
baseDir = "parser-tests" </> "Tests" </> "files"

verbosity :: Verbosity
verbosity = normal

readConfigDefault :: FilePath -> IO (ProjectConfigSkeleton, ProjectConfigSkeleton)
readConfigDefault testSubDir = readConfig testSubDir "cabal.project"

readConfig :: FilePath -> FilePath -> IO (ProjectConfigSkeleton, ProjectConfigSkeleton)
readConfig testSubDir projectFileName = do
  (TestDir testRootFp projectConfigFp distDirLayout) <- testDirInfo testSubDir projectFileName
  exists <- liftIO $ doesFileExist projectConfigFp
  assertBool ("projectConfig does not exist: " <> projectConfigFp) exists
  httpTransport <- liftIO $ configureTransport verbosity [] Nothing
  let extensionName = ""
      extensionDescription = ""
  parsec <-
    liftIO $
      runRebuild testRootFp $
        readProjectFileSkeleton verbosity httpTransport distDirLayout extensionName extensionDescription
  legacy <-
    liftIO $
      runRebuild testRootFp $
        readProjectFileSkeletonLegacy verbosity httpTransport distDirLayout extensionName extensionDescription
  return (parsec, legacy)

assertConfigEquals :: (Eq a, Show a) => a -> ProjectConfigSkeleton -> ProjectConfigSkeleton -> (ProjectConfigSkeleton -> a) -> Assertion
assertConfigEquals expected config configLegacy access = do
  assertEqual "Expectation does not match result of Legacy parser" expected actualLegacy
  assertEqual "Parsed Config does not match expected" expected actual
  where
    actual = access config
    actualLegacy = access configLegacy

-- | Represents the directory structure and associated file paths for a test
data TestDir = TestDir
  { _testDirTestRootFp :: FilePath
  -- ^ Every test has a root directory in ./files/<test-title>
  , _testDirProjectConfigFp :: FilePath
  -- ^ Every test has a project config in testDirTestRootFp/cabal.project
  , _testDirDistDirLayout :: DistDirLayout
  }

testDirInfo :: FilePath -> FilePath -> IO TestDir
testDirInfo testSubDir projectFileName = do
  projectRootDir <- canonicalizePath (baseDir </> testSubDir)
  let
    projectRoot = ProjectRootExplicit projectRootDir projectFileName
    distDirLayout = defaultDistDirLayout projectRoot Nothing Nothing
    extensionName = ""
    projectConfigFp = distProjectFile distDirLayout extensionName
  return $ TestDir projectRootDir projectConfigFp distDirLayout

-- | Compares two lists element-wise using a comparison function.
compareLists :: [a] -> [a] -> (a -> a -> Bool) -> Bool
compareLists xs ys compare' = length xs == length ys && all (uncurry compare') (zip xs ys)

-- | Compares LocalRepos ignoring field 'localRepoSharedCache' because we do not parse it.
compareLocalRepos :: LocalRepo -> LocalRepo -> Bool
compareLocalRepos repo1 repo2 =
  localRepoName repo1 == localRepoName repo2
    && localRepoPath repo1 == localRepoPath repo2

-- | Compares RemoteRepos ignoring field 'remoteRepoShouldTryHttps' because we do not parse it.
compareRemoteRepos :: RemoteRepo -> RemoteRepo -> Bool
compareRemoteRepos repo1 repo2 =
  remoteRepoName repo1 == remoteRepoName repo2
    && remoteRepoURI repo1 == remoteRepoURI repo2
    && remoteRepoSecure repo1 == remoteRepoSecure repo2
    && remoteRepoRootKeys repo1 == remoteRepoRootKeys repo2
    && remoteRepoKeyThreshold repo1 == remoteRepoKeyThreshold repo2
