{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.ByteString as BS
import Data.Either
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Distribution.Client.BuildReports.Types (ReportLevel (..))
import Distribution.Client.CmdInstall.ClientInstallFlags (ClientInstallFlags (..))
import Distribution.Client.Dependency.Types (PreSolver (..))
import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.IndexUtils.ActiveRepos (ActiveRepoEntry (..), ActiveRepos (..), CombineStrategy (..))
import Distribution.Client.IndexUtils.IndexState (RepoIndexState (..), headTotalIndexState, insertIndexState)
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Parsec
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.Client.Targets (readUserConstraint)
import Distribution.Client.Types.AllowNewer (AllowNewer (..), AllowOlder (..), RelaxDepMod (..), RelaxDepScope (..), RelaxDepSubject (..), RelaxDeps (..), RelaxedDep (..))
import Distribution.Client.Types.InstallMethod (InstallMethod (..))
import Distribution.Client.Types.OverwritePolicy (OverwritePolicy (..))
import Distribution.Client.Types.Repo (LocalRepo (..), RemoteRepo (..))
import Distribution.Client.Types.RepoName (RepoName (..))
import Distribution.Client.Types.SourceRepo
import Distribution.Client.Types.WriteGhcEnvironmentFilesPolicy (WriteGhcEnvironmentFilesPolicy (..))
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Parsec (simpleParsec)
import Distribution.Simple.Compiler (DebugInfoLevel (..), OptimisationLevel (..), PackageDB (..), ProfDetailLevel (..))
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs (InstallDirs (..), toPathTemplate)
import Distribution.Simple.Setup (DumpBuildInfo (..), Flag, HaddockTarget (..), TestShowDetails (..))
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
import Distribution.Types.Flag (FlagAssignment (..), FlagName, mkFlagAssignment)
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.PackageName
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Types.SourceRepo (KnownRepoType (..), RepoType (..))
import Distribution.Types.Version (mkVersion)
import Distribution.Types.VersionRange.Internal (VersionRange (..))
import Distribution.Utils.NubList
import Distribution.Verbosity
import Network.URI (parseURI)
import System.Directory
import System.FilePath

import Test.Cabal.Prelude hiding (cabal)
import qualified Test.Cabal.Prelude as P

main = do
  cabalTest' "read packages" testPackages
  cabalTest' "read optional-packages" testOptionalPackages
  cabalTest' "read extra-packages" testExtraPackages
  cabalTest' "read source-repository-package" testSourceRepoList
  cabalTest' "read project-config-build-only" testProjectConfigBuildOnly
  cabalTest' "read project-config-shared" testProjectConfigShared
  cabalTest' "read install-dirs" testInstallDirs
  cabalTest' "read remote-repos" testRemoteRepos
  cabalTest' "read local-no-index-repos" testLocalNoIndexRepos
  cabalTest' "set explicit provenance" testProjectConfigProvenance
  cabalTest' "read project-config-local-packages" testProjectConfigLocalPackages
  cabalTest' "read project-config-all-packages" testProjectConfigAllPackages
  cabalTest' "read project-config-specific-packages" testProjectConfigSpecificPackages
  cabalTest' "test projectConfigAllPackages concatenation" testAllPackagesConcat
  cabalTest' "test projectConfigSpecificPackages concatenation" testSpecificPackagesConcat
  cabalTest' "test program-locations concatenation" testProgramLocationsConcat
  cabalTest' "test program-options concatenation" testProgramOptionsConcat

testPackages :: TestM ()
testPackages = do
  let expected = [".", "packages/packages.cabal"]
  -- Note that I currently also run the legacy parser to make sure my expected values
  -- do not differ from the non-Parsec implementation, this will be removed in the future
  (config, legacy) <- readConfigDefault "packages"
  assertConfig expected config legacy (projectPackages . condTreeData)

testOptionalPackages :: TestM ()
testOptionalPackages = do
  let expected = [".", "packages/packages.cabal"]
  (config, legacy) <- readConfigDefault "optional-packages"
  assertConfig expected config legacy (projectPackagesOptional . condTreeData)

testSourceRepoList :: TestM ()
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

testExtraPackages :: TestM ()
testExtraPackages = do
  let expected =
        [ PackageVersionConstraint (mkPackageName "a") (OrLaterVersion (mkVersion [0]))
        , PackageVersionConstraint (mkPackageName "b") (IntersectVersionRanges (OrLaterVersion (mkVersion [0, 7, 3])) (EarlierVersion (mkVersion [0, 9])))
        ]
  (config, legacy) <- readConfigDefault "extra-packages"
  assertConfig expected config legacy (projectPackagesNamed . condTreeData)

testProjectConfigBuildOnly :: TestM ()
testProjectConfigBuildOnly = do
  let expected = ProjectConfigBuildOnly{..}
  (config, legacy) <- readConfigDefault "project-config-build-only"
  assertConfig expected config legacy (projectConfigBuildOnly . condTreeData)
  where
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

testProjectConfigShared :: TestM ()
testProjectConfigShared = do
  let rootFp = "project-config-shared"
  testDir <- testDirInfo rootFp "cabal.project"
  let
    projectConfigConstraints = getProjectConfigConstraints (testDirProjectConfigFp testDir)
    expected = ProjectConfigShared{..}
  (config, legacy) <- readConfigDefault rootFp
  assertConfig expected config legacy (projectConfigShared . condTreeData)
  where
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
    getProjectConfigConstraints projectFileFp =
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

testInstallDirs :: TestM ()
testInstallDirs = do
  let rootFp = "install-dirs"
  (config, legacy) <- readConfigDefault rootFp
  assertConfig expected config legacy (projectConfigInstallDirs . projectConfigShared . condTreeData)
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

testRemoteRepos :: TestM ()
testRemoteRepos = do
  let rootFp = "remote-repos"
  (config, legacy) <- readConfigDefault rootFp
  let actualRemoteRepos = (fromNubList . projectConfigRemoteRepos . projectConfigShared . condTreeData) config
  assertBool "Expected RemoteRepos do not match parsed values" $ compareLists expected actualRemoteRepos compareRemoteRepos
  assertConfig mempty config legacy (projectConfigLocalNoIndexRepos . projectConfigShared . condTreeData)
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

-- We do not parse remoteRepoShouldTryHttps, so we skip it
compareRemoteRepos :: RemoteRepo -> RemoteRepo -> Bool
compareRemoteRepos repo1 repo2 =
  remoteRepoName repo1 == remoteRepoName repo2
    && remoteRepoURI repo1 == remoteRepoURI repo2
    && remoteRepoSecure repo1 == remoteRepoSecure repo2
    && remoteRepoRootKeys repo1 == remoteRepoRootKeys repo2
    && remoteRepoKeyThreshold repo1 == remoteRepoKeyThreshold repo2

testLocalNoIndexRepos :: TestM ()
testLocalNoIndexRepos = do
  let rootFp = "local-no-index-repos"
  (config, legacy) <- readConfigDefault rootFp
  let actualLocalRepos = (fromNubList . projectConfigLocalNoIndexRepos . projectConfigShared . condTreeData) config
  assertBool "Expected LocalNoIndexRepos do not match parsed values" $ compareLists expected actualLocalRepos compareLocalRepos
  assertConfig mempty config legacy (projectConfigRemoteRepos . projectConfigShared . condTreeData)
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

-- We do not parse localRepoSharedCache, so we skip it
compareLocalRepos :: LocalRepo -> LocalRepo -> Bool
compareLocalRepos repo1 repo2 =
  localRepoName repo1 == localRepoName repo2
    && localRepoPath repo1 == localRepoPath repo2

testProjectConfigProvenance :: TestM ()
testProjectConfigProvenance = do
  let rootFp = "empty"
  let
    expected = Set.singleton (Explicit (ProjectConfigPath $ "cabal.project" :| []))
  (config, legacy) <- readConfigDefault rootFp
  assertConfig expected config legacy (projectConfigProvenance . condTreeData)

testProjectConfigLocalPackages :: TestM ()
testProjectConfigLocalPackages = do
  let rootFp = "project-config-local-packages"
  let expected = PackageConfig{..}
  (config, legacy) <- readConfigDefault rootFp
  assertConfig expected config legacy (projectConfigLocalPackages . condTreeData)
  where
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
    packageConfigHaddockForHackage = Flag ForHackage
    packageConfigTestHumanLog = Flag $ toPathTemplate "human-log.log"
    packageConfigTestMachineLog = Flag $ toPathTemplate "machine.log"
    packageConfigTestShowDetails = Flag Streaming
    packageConfigTestKeepTix = Flag True
    packageConfigTestWrapper = Flag "/test-wrapper-path/"
    packageConfigTestFailWhenNoTestSuites = Flag True
    packageConfigTestTestOptions = [toPathTemplate "--some-option", toPathTemplate "42"]
    packageConfigBenchmarkOptions = [toPathTemplate "--some-benchmark-option", toPathTemplate "--another-option"]

testProjectConfigAllPackages :: TestM ()
testProjectConfigAllPackages = do
  let rootFp = "project-config-all-packages"
  (config, legacy) <- readConfigDefault rootFp
  assertConfig expected config legacy (projectConfigAllPackages . condTreeData)
  where
    expected :: PackageConfig
    expected =
      mempty
        { packageConfigProfDetail = Flag ProfDetailAllFunctions
        , packageConfigProfLibDetail = Flag ProfDetailExportedFunctions
        }

testProjectConfigSpecificPackages :: TestM ()
testProjectConfigSpecificPackages = do
  let rootFp = "project-config-specific-packages"
  (config, legacy) <- readConfigDefault rootFp
  assertConfig expected config legacy (projectConfigSpecificPackage . condTreeData)
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

testAllPackagesConcat :: TestM ()
testAllPackagesConcat = do
  let rootFp = "all-packages-concat"
  (config, legacy) <- readConfigDefault rootFp
  assertConfig expected config legacy (projectConfigAllPackages . condTreeData)
  where
    expected :: PackageConfig
    expected =
      mempty
        { packageConfigSharedLib = Flag True
        , packageConfigStaticLib = Flag True
        , packageConfigProgramArgs = MapMappend $ Map.fromList [("ghc", ["-fwarn-tabs", "-Wall"])]
        }

testSpecificPackagesConcat :: TestM ()
testSpecificPackagesConcat = do
  let rootFp = "specific-packages-concat"
  (config, legacy) <- readConfigDefault rootFp
  assertConfig expected config legacy (projectConfigSpecificPackage . condTreeData)
  where
    expected = MapMappend $ Map.fromList [("foo", expectedFoo)]
    expectedFoo :: PackageConfig
    expectedFoo =
      mempty
        { packageConfigSharedLib = Flag True
        , packageConfigStaticLib = Flag True
        , packageConfigProgramArgs = MapMappend $ Map.fromList [("ghc", ["-fno-state-hack", "-threaded"])]
        }

testProgramLocationsConcat :: TestM ()
testProgramLocationsConcat = do
  let rootFp = "program-locations-concat"
  (config, legacy) <- readConfigDefault rootFp
  assertConfig expected config legacy (projectConfigLocalPackages . condTreeData)
  where
    expected :: PackageConfig
    expected =
      mempty
        { packageConfigProgramPaths = MapLast $ Map.fromList [("gcc", "/tmp/bin/gcc"), ("ghc", "/tmp/bin/ghc")]
        }

testProgramOptionsConcat :: TestM ()
testProgramOptionsConcat = do
  let rootFp = "program-options-concat"
  (config, legacy) <- readConfigDefault rootFp
  assertConfig expected config legacy (projectConfigLocalPackages . condTreeData)
  where
    expected :: PackageConfig
    expected =
      mempty
        { packageConfigProgramArgs = MapMappend $ Map.fromList [("ghc", ["-threaded", "-Wall", "-fno-state-hack"]), ("gcc", ["-baz", "-foo", "-bar"])]
        }

readConfigDefault :: FilePath -> TestM (ProjectConfigSkeleton, ProjectConfigSkeleton)
readConfigDefault testSubDir = readConfig testSubDir "cabal.project"

readConfig :: FilePath -> FilePath -> TestM (ProjectConfigSkeleton, ProjectConfigSkeleton)
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

data TestDir = TestDir
  { testDirTestRootFp :: FilePath
  -- ^ Every test has its own root in ./tests/<test-title>
  , testDirProjectConfigFp :: FilePath
  -- ^ Every test has a project config in testDirTestRootFp/cabal.project
  , testDirDistDirLayout :: DistDirLayout
  }

testDirInfo :: FilePath -> FilePath -> TestM TestDir
testDirInfo testSubDir projectFileName = do
  testEnv <- getTestEnv
  testRootFp <- liftIO $ canonicalizePath (testCurrentDir testEnv </> "tests" </> testSubDir)
  let
    projectRoot = ProjectRootExplicit testRootFp projectFileName
    distDirLayout = defaultDistDirLayout projectRoot Nothing Nothing
    extensionName = ""
    projectConfigFp = distProjectFile distDirLayout extensionName
  return $ TestDir testRootFp projectConfigFp distDirLayout
  where
    extensionName = ""

assertConfig :: (Eq a, Show a) => a -> ProjectConfigSkeleton -> ProjectConfigSkeleton -> (ProjectConfigSkeleton -> a) -> TestM ()
assertConfig expected config configLegacy access = do
  assertEqual "Expectation does not match result of Legacy parser" expected actualLegacy
  assertEqual "Parsed Config does not match expected" expected actual
  where
    actual = access config
    actualLegacy = access configLegacy

-- | Test Utilities
verbosity :: Verbosity
verbosity = normal -- minBound --normal --verbose --maxBound --minBound

compareLists :: [a] -> [a] -> (a -> a -> Bool) -> Bool
compareLists xs ys compare = length xs == length ys && all (uncurry compare) (zip xs ys)
