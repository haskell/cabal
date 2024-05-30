{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- simplifier goes nuts otherwise
#if __GLASGOW_HASKELL__ < 806
{-# OPTIONS_GHC -funfolding-use-threshold=30 #-}
#endif

module UnitTests.Distribution.Client.ProjectConfig (tests) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
import Control.Applicative
#endif
import Control.Monad
import Data.Either (isRight)
import Data.Foldable (for_)
import Data.List (intercalate, isPrefixOf, (\\))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Network.URI (URI)
import System.Directory (canonicalizePath, withCurrentDirectory)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

import Distribution.Deprecated.ParseUtils
import qualified Distribution.Deprecated.ReadP as Parse

import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Types
import Distribution.Simple.Utils (toUTF8BS)
import Distribution.Types.PackageVersionConstraint
import Distribution.Version

import Distribution.Parsec
import Distribution.Pretty

import Distribution.Client.CmdInstall.ClientInstallFlags
import Distribution.Client.Dependency.Types
import Distribution.Client.DistDirLayout (defaultProjectFile)
import Distribution.Client.Targets
import Distribution.Client.Types
import Distribution.Client.Types.SourceRepo
import Distribution.Utils.NubList
import Distribution.Verbosity (silent)

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.ProjectConfigPath
import Distribution.Solver.Types.Settings

import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Legacy

import UnitTests.Distribution.Client.ArbitraryInstances
import UnitTests.Distribution.Client.TreeDiffInstances ()

import Data.TreeDiff.Class
import Data.TreeDiff.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testGroup "ProjectConfig <-> LegacyProjectConfig round trip" $
      [ testProperty "packages" prop_roundtrip_legacytypes_packages
      , testProperty "buildonly" prop_roundtrip_legacytypes_buildonly
      , testProperty "specific" prop_roundtrip_legacytypes_specific
      ]
        ++
        -- a couple tests seem to trigger a RTS fault in ghc-7.6 and older
        -- unclear why as of yet
        concat
          [ [ testProperty "shared" prop_roundtrip_legacytypes_shared
            , testProperty "local" prop_roundtrip_legacytypes_local
            , testProperty "all" prop_roundtrip_legacytypes_all
            ]
          | not usingGhc76orOlder
          ]
  , testGroup
      "individual parser tests"
      [ testProperty "package location" prop_parsePackageLocationTokenQ
      , testProperty "RelaxedDep" prop_roundtrip_printparse_RelaxedDep
      , testProperty "RelaxDeps" prop_roundtrip_printparse_RelaxDeps
      , testProperty "RelaxDeps'" prop_roundtrip_printparse_RelaxDeps'
      ]
  , testGroup
      "ProjectConfig printing/parsing round trip"
      [ testProperty "packages" prop_roundtrip_printparse_packages
      , testProperty "buildonly" prop_roundtrip_printparse_buildonly
      , testProperty "shared" prop_roundtrip_printparse_shared
      , testProperty "local" prop_roundtrip_printparse_local
      , testProperty "specific" prop_roundtrip_printparse_specific
      , testProperty "all" prop_roundtrip_printparse_all
      ]
  , testFindProjectRoot
  ]
  where
    usingGhc76orOlder =
      case buildCompilerId of
        CompilerId GHC v -> v < mkVersion [7, 7]
        _ -> False

testFindProjectRoot :: TestTree
testFindProjectRoot =
  testGroup
    "findProjectRoot"
    [ test "defaults" (cd dir) Nothing Nothing (succeeds dir file)
    , test "defaults in lib" (cd libDir) Nothing Nothing (succeeds dir file)
    , test "explicit file" (cd dir) Nothing (Just file) (succeeds dir file)
    , test "explicit file in lib" (cd libDir) Nothing (Just file) (succeeds dir file)
    , test "other file" (cd dir) Nothing (Just fileOther) (succeeds dir fileOther)
    , test "other file in lib" (cd libDir) Nothing (Just fileOther) (succeeds dir fileOther)
    , -- Deprecated use-case
      test "absolute file" Nothing Nothing (Just absFile) (succeeds dir file)
    , test "nested file" (cd dir) Nothing (Just nixFile) (succeeds dir nixFile)
    , test "nested file in lib" (cd libDir) Nothing (Just nixFile) (succeeds dir nixFile)
    , test "explicit dir" Nothing (Just dir) Nothing (succeeds dir file)
    , test "explicit dir & file" Nothing (Just dir) (Just file) (succeeds dir file)
    , test "explicit dir & nested file" Nothing (Just dir) (Just nixFile) (succeeds dir nixFile)
    , test "explicit dir & nested other file" Nothing (Just dir) (Just nixOther) (succeeds dir nixOther)
    , test "explicit dir & absolute file" Nothing (Just dir) (Just absFile) (succeedsWith ProjectRootExplicitAbsolute dir absFile)
    ]
  where
    dir = fixturesDir </> "project-root"
    libDir = dir </> "lib"

    file = defaultProjectFile
    fileOther = file <.> "other"
    absFile = dir </> file

    nixFile = "nix" </> file
    nixOther = nixFile <.> "other"

    missing path = Just (path <.> "does_not_exist")

    test name wrap projectDir projectFile validate =
      testCaseSteps name $ \step -> fromMaybe id wrap $ do
        result <- findProjectRoot silent projectDir projectFile
        _ <- validate result

        when (isRight result) $ do
          for_ projectDir $ \path -> do
            step "missing project dir"
            fails =<< findProjectRoot silent (missing path) projectFile

          for_ projectFile $ \path -> do
            step "missing project file"
            fails =<< findProjectRoot silent projectDir (missing path)

    cd d = Just (withCurrentDirectory d)

    succeeds = succeedsWith ProjectRootExplicit

    succeedsWith mk projectDir projectFile result = case result of
      Left err -> assertFailure $ "Expected ProjectRoot, but found " <> show err
      Right pr -> pr @?= mk projectDir projectFile

    fails result = case result of
      Left _ -> pure ()
      Right x -> assertFailure $ "Expected an error, but found " <> show x

fixturesDir :: FilePath
fixturesDir =
  unsafePerformIO $
    canonicalizePath ("tests" </> "fixtures")
{-# NOINLINE fixturesDir #-}

------------------------------------------------
-- Round trip: conversion to/from legacy types
--

roundtrip :: (Eq a, ToExpr a, Show b) => (a -> b) -> (b -> a) -> a -> Property
roundtrip f f_inv x =
  counterexample (show y) $
    x `ediffEq` f_inv y -- no counterexample with y, as they not have ToExpr
  where
    y = f x

roundtrip_legacytypes :: ProjectConfig -> Property
roundtrip_legacytypes =
  roundtrip
    convertToLegacyProjectConfig
    convertLegacyProjectConfig

prop_roundtrip_legacytypes_all :: ProjectConfig -> Property
prop_roundtrip_legacytypes_all config =
  roundtrip_legacytypes
    config
      { projectConfigProvenance = mempty
      }

prop_roundtrip_legacytypes_packages :: ProjectConfig -> Property
prop_roundtrip_legacytypes_packages config =
  roundtrip_legacytypes
    config
      { projectConfigBuildOnly = mempty
      , projectConfigShared = mempty
      , projectConfigProvenance = mempty
      , projectConfigLocalPackages = mempty
      , projectConfigSpecificPackage = mempty
      }

prop_roundtrip_legacytypes_buildonly :: ProjectConfigBuildOnly -> Property
prop_roundtrip_legacytypes_buildonly config =
  roundtrip_legacytypes
    mempty{projectConfigBuildOnly = config}

prop_roundtrip_legacytypes_shared :: ProjectConfigShared -> Property
prop_roundtrip_legacytypes_shared config =
  roundtrip_legacytypes
    mempty{projectConfigShared = config}

prop_roundtrip_legacytypes_local :: PackageConfig -> Property
prop_roundtrip_legacytypes_local config =
  roundtrip_legacytypes
    mempty{projectConfigLocalPackages = config}

prop_roundtrip_legacytypes_specific :: Map PackageName PackageConfig -> Property
prop_roundtrip_legacytypes_specific config =
  roundtrip_legacytypes
    mempty{projectConfigSpecificPackage = MapMappend config}

--------------------------------------------
-- Round trip: printing and parsing config
--

roundtrip_printparse :: ProjectConfig -> Property
roundtrip_printparse config =
  case fmap convertLegacyProjectConfig (parseLegacyProjectConfig "unused" (toUTF8BS str)) of
    ParseOk _ x ->
      counterexample ("shown:\n" ++ str) $
        x `ediffEq` config{projectConfigProvenance = mempty}
    ParseFailed err -> counterexample ("shown:\n" ++ str ++ "\nERROR: " ++ show err) False
  where
    str :: String
    str = showLegacyProjectConfig (convertToLegacyProjectConfig config)

prop_roundtrip_printparse_all :: ProjectConfig -> Property
prop_roundtrip_printparse_all config =
  roundtrip_printparse
    config
      { projectConfigBuildOnly =
          hackProjectConfigBuildOnly (projectConfigBuildOnly config)
      , projectConfigShared =
          hackProjectConfigShared (projectConfigShared config)
      }

prop_roundtrip_printparse_packages
  :: [PackageLocationString]
  -> [PackageLocationString]
  -> [SourceRepoList]
  -> [PackageVersionConstraint]
  -> Property
prop_roundtrip_printparse_packages pkglocstrs1 pkglocstrs2 repos named =
  roundtrip_printparse
    mempty
      { projectPackages = map getPackageLocationString pkglocstrs1
      , projectPackagesOptional = map getPackageLocationString pkglocstrs2
      , projectPackagesRepo = repos
      , projectPackagesNamed = named
      }

prop_roundtrip_printparse_buildonly :: ProjectConfigBuildOnly -> Property
prop_roundtrip_printparse_buildonly config =
  roundtrip_printparse
    mempty
      { projectConfigBuildOnly = hackProjectConfigBuildOnly config
      }

hackProjectConfigBuildOnly :: ProjectConfigBuildOnly -> ProjectConfigBuildOnly
hackProjectConfigBuildOnly config =
  config
    { -- These fields are only command line transitory things, not
      -- something to be recorded persistently in a config file
      projectConfigOnlyDeps = mempty
    , projectConfigOnlyDownload = mempty
    , projectConfigDryRun = mempty
    }

prop_roundtrip_printparse_shared :: ProjectConfigShared -> Property
prop_roundtrip_printparse_shared config =
  roundtrip_printparse
    mempty
      { projectConfigShared = hackProjectConfigShared config
      }

hackProjectConfigShared :: ProjectConfigShared -> ProjectConfigShared
hackProjectConfigShared config =
  config
    { projectConfigProjectFile = mempty -- not present within project files
    , projectConfigProjectDir = mempty -- ditto
    , projectConfigConfigFile = mempty -- ditto
    , projectConfigConstraints =
        -- TODO: [required eventually] parse ambiguity in constraint
        -- "pkgname -any" as either any version or disabled flag "any".
        let ambiguous (UserConstraint _ (PackagePropertyFlags flags), _) =
              (not . null)
                [ () | (name, False) <- unFlagAssignment flags, "any" `isPrefixOf` unFlagName name
                ]
            ambiguous _ = False
         in filter (not . ambiguous) (projectConfigConstraints config)
    }

prop_roundtrip_printparse_local :: PackageConfig -> Property
prop_roundtrip_printparse_local config =
  roundtrip_printparse
    mempty
      { projectConfigLocalPackages = config
      }

prop_roundtrip_printparse_specific
  :: Map PackageName (NonMEmpty PackageConfig)
  -> Property
prop_roundtrip_printparse_specific config =
  roundtrip_printparse
    mempty
      { projectConfigSpecificPackage = MapMappend (fmap getNonMEmpty config)
      }

----------------------------
-- Individual Parser tests
--

-- | Helper to parse a given string
--
-- Succeeds only if there is a unique complete parse
runReadP :: Parse.ReadP a a -> String -> Maybe a
runReadP parser s = case [x | (x, "") <- Parse.readP_to_S parser s] of
  [x'] -> Just x'
  _ -> Nothing

prop_parsePackageLocationTokenQ :: PackageLocationString -> Bool
prop_parsePackageLocationTokenQ (PackageLocationString str) =
  runReadP parsePackageLocationTokenQ (renderPackageLocationToken str) == Just str

prop_roundtrip_printparse_RelaxedDep :: RelaxedDep -> Property
prop_roundtrip_printparse_RelaxedDep rdep =
  counterexample (prettyShow rdep) $
    eitherParsec (prettyShow rdep) == Right rdep

prop_roundtrip_printparse_RelaxDeps :: RelaxDeps -> Property
prop_roundtrip_printparse_RelaxDeps rdep =
  counterexample (prettyShow rdep) $
    Right rdep `ediffEq` eitherParsec (prettyShow rdep)

prop_roundtrip_printparse_RelaxDeps' :: RelaxDeps -> Property
prop_roundtrip_printparse_RelaxDeps' rdep =
  counterexample rdep' $
    Right rdep `ediffEq` eitherParsec rdep'
  where
    rdep' = go (prettyShow rdep)

    -- replace 'all' tokens by '*'
    go :: String -> String
    go [] = []
    go "all" = "*"
    go ('a' : 'l' : 'l' : c : rest) | c `elem` ":," = '*' : go (c : rest)
    go rest =
      let (x, y) = break (`elem` ":,") rest
          (x', y') = span (`elem` ":,^") y
       in x ++ x' ++ go y'

------------------------
-- Arbitrary instances
--

instance Arbitrary ProjectConfig where
  arbitrary =
    ProjectConfig
      <$> (map getPackageLocationString <$> arbitrary)
      <*> (map getPackageLocationString <$> arbitrary)
      <*> shortListOf 3 arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> ( MapMappend . fmap getNonMEmpty . Map.fromList
              <$> shortListOf 3 arbitrary
          )

  -- package entries with no content are equivalent to
  -- the entry not existing at all, so exclude empty

  shrink
    ProjectConfig
      { projectPackages = x0
      , projectPackagesOptional = x1
      , projectPackagesRepo = x2
      , projectPackagesNamed = x3
      , projectConfigBuildOnly = x4
      , projectConfigShared = x5
      , projectConfigProvenance = x6
      , projectConfigLocalPackages = x7
      , projectConfigSpecificPackage = x8
      , projectConfigAllPackages = x9
      } =
      [ ProjectConfig
        { projectPackages = x0'
        , projectPackagesOptional = x1'
        , projectPackagesRepo = x2'
        , projectPackagesNamed = x3'
        , projectConfigBuildOnly = x4'
        , projectConfigShared = x5'
        , projectConfigProvenance = x6'
        , projectConfigLocalPackages = x7'
        , projectConfigSpecificPackage =
            ( MapMappend
                (fmap getNonMEmpty x8')
            )
        , projectConfigAllPackages = x9'
        }
      | ((x0', x1', x2', x3'), (x4', x5', x6', x7', x8', x9')) <-
          shrink
            ( (x0, x1, x2, x3)
            , (x4, x5, x6, x7, fmap NonMEmpty (getMapMappend x8), x9)
            )
      ]

newtype PackageLocationString = PackageLocationString {getPackageLocationString :: String}
  deriving (Show)

instance Arbitrary PackageLocationString where
  arbitrary =
    PackageLocationString
      <$> oneof
        [ show . getNonEmpty <$> (arbitrary :: Gen (NonEmptyList String))
        , arbitraryGlobLikeStr
        , show <$> (arbitrary :: Gen URI)
        ]
        `suchThat` (\xs -> not ("{" `isPrefixOf` xs))

arbitraryGlobLikeStr :: Gen String
arbitraryGlobLikeStr = outerTerm
  where
    outerTerm =
      concat
        <$> shortListOf1
          4
          ( frequency
              [ (2, token)
              , (1, braces <$> innerTerm)
              ]
          )
    innerTerm =
      intercalate ","
        <$> shortListOf1
          3
          ( frequency
              [ (3, token)
              , (1, braces <$> innerTerm)
              ]
          )
    token = shortListOf1 4 (elements (['#' .. '~'] \\ "{,}"))
    braces s = "{" ++ s ++ "}"

instance Arbitrary ClientInstallFlags where
  arbitrary =
    ClientInstallFlags
      <$> arbitrary
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryFlag arbitraryShortToken

instance Arbitrary ProjectConfigBuildOnly where
  arbitrary =
    ProjectConfigBuildOnly
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (toNubList <$> shortListOf 2 arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (fmap getShortToken <$> arbitrary)
      <*> arbitraryNumJobs
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (fmap getShortToken <$> arbitrary)
      <*> arbitrary
      <*> (fmap getShortToken <$> arbitrary)
      <*> (fmap getShortToken <$> arbitrary)
      <*> arbitrary
    where
      arbitraryNumJobs = fmap (fmap getPositive) <$> arbitrary

  shrink
    ProjectConfigBuildOnly
      { projectConfigVerbosity = x00
      , projectConfigDryRun = x01
      , projectConfigOnlyDeps = x02
      , projectConfigOnlyDownload = x18
      , projectConfigSummaryFile = x03
      , projectConfigLogFile = x04
      , projectConfigBuildReports = x05
      , projectConfigReportPlanningFailure = x06
      , projectConfigSymlinkBinDir = x07
      , projectConfigNumJobs = x09
      , projectConfigUseSemaphore = x19
      , projectConfigKeepGoing = x10
      , projectConfigOfflineMode = x11
      , projectConfigKeepTempFiles = x12
      , projectConfigHttpTransport = x13
      , projectConfigIgnoreExpiry = x14
      , projectConfigCacheDir = x15
      , projectConfigLogsDir = x16
      , projectConfigClientInstallFlags = x17
      } =
      [ ProjectConfigBuildOnly
        { projectConfigVerbosity = x00'
        , projectConfigDryRun = x01'
        , projectConfigOnlyDeps = x02'
        , projectConfigOnlyDownload = x18'
        , projectConfigSummaryFile = x03'
        , projectConfigLogFile = x04'
        , projectConfigBuildReports = x05'
        , projectConfigReportPlanningFailure = x06'
        , projectConfigSymlinkBinDir = x07'
        , projectConfigNumJobs = postShrink_NumJobs x09'
        , projectConfigUseSemaphore = x19'
        , projectConfigKeepGoing = x10'
        , projectConfigOfflineMode = x11'
        , projectConfigKeepTempFiles = x12'
        , projectConfigHttpTransport = x13
        , projectConfigIgnoreExpiry = x14'
        , projectConfigCacheDir = x15
        , projectConfigLogsDir = x16
        , projectConfigClientInstallFlags = x17'
        }
      | ( (x00', x01', x02', x03', x04')
          , (x05', x06', x07', x09')
          , (x10', x11', x12', x14')
          , (x17', x18', x19')
          ) <-
          shrink
            ( (x00, x01, x02, x03, x04)
            , (x05, x06, x07, preShrink_NumJobs x09)
            , (x10, x11, x12, x14)
            , (x17, x18, x19)
            )
      ]
      where
        preShrink_NumJobs = fmap (fmap Positive)
        postShrink_NumJobs = fmap (fmap getPositive)

instance Arbitrary ProjectConfigShared where
  arbitrary = do
    projectConfigDistDir <- arbitraryFlag arbitraryShortToken
    projectConfigConfigFile <- arbitraryFlag arbitraryShortToken
    projectConfigProjectDir <- arbitraryFlag arbitraryShortToken
    projectConfigProjectFile <- arbitraryFlag arbitraryShortToken
    projectConfigIgnoreProject <- arbitrary
    projectConfigHcFlavor <- arbitrary
    projectConfigHcPath <- arbitraryFlag arbitraryShortToken
    projectConfigHcPkg <- arbitraryFlag arbitraryShortToken
    projectConfigHaddockIndex <- arbitrary
    projectConfigInstallDirs <- fixInstallDirs <$> arbitrary
    projectConfigPackageDBs <- shortListOf 2 arbitrary
    projectConfigRemoteRepos <- arbitrary
    projectConfigLocalNoIndexRepos <- arbitrary
    projectConfigActiveRepos <- arbitrary
    projectConfigIndexState <- arbitrary
    projectConfigStoreDir <- arbitraryFlag arbitraryShortToken
    projectConfigConstraints <- arbitraryConstraints
    projectConfigPreferences <- shortListOf 2 arbitrary
    projectConfigCabalVersion <- arbitrary
    projectConfigSolver <- arbitrary
    projectConfigAllowOlder <- arbitrary
    projectConfigAllowNewer <- arbitrary
    projectConfigWriteGhcEnvironmentFilesPolicy <- arbitrary
    projectConfigMaxBackjumps <- arbitrary
    projectConfigReorderGoals <- arbitrary
    projectConfigCountConflicts <- arbitrary
    projectConfigFineGrainedConflicts <- arbitrary
    projectConfigMinimizeConflictSet <- arbitrary
    projectConfigStrongFlags <- arbitrary
    projectConfigAllowBootLibInstalls <- arbitrary
    projectConfigOnlyConstrained <- arbitrary
    projectConfigPerComponent <- arbitrary
    projectConfigIndependentGoals <- arbitrary
    projectConfigPreferOldest <- arbitrary
    projectConfigProgPathExtra <- toNubList <$> listOf arbitraryShortToken
    projectConfigMultiRepl <- arbitrary
    return ProjectConfigShared{..}
    where
      arbitraryConstraints :: Gen [(UserConstraint, ConstraintSource)]
      arbitraryConstraints =
        fmap (\uc -> (uc, projectConfigConstraintSource)) <$> arbitrary
      fixInstallDirs x = x{InstallDirs.includedir = mempty, InstallDirs.mandir = mempty, InstallDirs.flibdir = mempty}

  shrink ProjectConfigShared{..} =
    runShrinker $
      pure ProjectConfigShared
        <*> shrinker projectConfigDistDir
        <*> shrinker projectConfigConfigFile
        <*> shrinker projectConfigProjectDir
        <*> shrinker projectConfigProjectFile
        <*> shrinker projectConfigIgnoreProject
        <*> shrinker projectConfigHcFlavor
        <*> shrinkerAla (fmap NonEmpty) projectConfigHcPath
        <*> shrinkerAla (fmap NonEmpty) projectConfigHcPkg
        <*> shrinker projectConfigHaddockIndex
        <*> shrinker projectConfigInstallDirs
        <*> shrinker projectConfigPackageDBs
        <*> shrinker projectConfigRemoteRepos
        <*> shrinker projectConfigLocalNoIndexRepos
        <*> shrinker projectConfigActiveRepos
        <*> shrinker projectConfigIndexState
        <*> shrinker projectConfigStoreDir
        <*> shrinkerPP preShrink_Constraints postShrink_Constraints projectConfigConstraints
        <*> shrinker projectConfigPreferences
        <*> shrinker projectConfigCabalVersion
        <*> shrinker projectConfigSolver
        <*> shrinker projectConfigAllowOlder
        <*> shrinker projectConfigAllowNewer
        <*> shrinker projectConfigWriteGhcEnvironmentFilesPolicy
        <*> shrinker projectConfigMaxBackjumps
        <*> shrinker projectConfigReorderGoals
        <*> shrinker projectConfigCountConflicts
        <*> shrinker projectConfigFineGrainedConflicts
        <*> shrinker projectConfigMinimizeConflictSet
        <*> shrinker projectConfigStrongFlags
        <*> shrinker projectConfigAllowBootLibInstalls
        <*> shrinker projectConfigOnlyConstrained
        <*> shrinker projectConfigPerComponent
        <*> shrinker projectConfigIndependentGoals
        <*> shrinker projectConfigPreferOldest
        <*> shrinker projectConfigProgPathExtra
        <*> shrinker projectConfigMultiRepl
    where
      preShrink_Constraints = map fst
      postShrink_Constraints = map (\uc -> (uc, projectConfigConstraintSource))

projectConfigConstraintSource :: ConstraintSource
projectConfigConstraintSource = ConstraintSourceProjectConfig nullProjectConfigPath

instance Arbitrary ProjectConfigProvenance where
  arbitrary = elements [Implicit, Explicit (ProjectConfigPath $ "cabal.project" :| [])]

instance Arbitrary PackageConfig where
  arbitrary =
    PackageConfig
      <$> ( MapLast . Map.fromList
              <$> shortListOf
                10
                ( (,)
                    <$> arbitraryProgramName
                    <*> arbitraryShortToken
                )
          )
      <*> ( MapMappend . Map.fromList
              <$> shortListOf
                10
                ( (,)
                    <$> arbitraryProgramName
                    <*> listOf arbitraryShortToken
                )
          )
      <*> (toNubList <$> listOf arbitraryShortToken)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> shortListOf 5 arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> shortListOf 5 arbitraryShortToken
      <*> shortListOf 5 arbitraryShortToken
      <*> shortListOf 5 arbitraryShortToken
      <*> shortListOf 5 arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryFlag arbitraryShortToken
      <*> arbitrary
      <*> shortListOf 5 arbitrary
      <*> shortListOf 5 arbitrary
    where
      arbitraryProgramName :: Gen String
      arbitraryProgramName =
        elements
          [ programName prog
          | (prog, _) <- knownPrograms (defaultProgramDb)
          ]

  shrink
    PackageConfig
      { packageConfigProgramPaths = x00
      , packageConfigProgramArgs = x01
      , packageConfigProgramPathExtra = x02
      , packageConfigFlagAssignment = x03
      , packageConfigVanillaLib = x04
      , packageConfigSharedLib = x05
      , packageConfigStaticLib = x42
      , packageConfigDynExe = x06
      , packageConfigFullyStaticExe = x50
      , packageConfigProf = x07
      , packageConfigProfLib = x08
      , packageConfigProfExe = x09
      , packageConfigProfDetail = x10
      , packageConfigProfLibDetail = x11
      , packageConfigConfigureArgs = x12
      , packageConfigOptimization = x13
      , packageConfigProgPrefix = x14
      , packageConfigProgSuffix = x15
      , packageConfigExtraLibDirs = x16
      , packageConfigExtraLibDirsStatic = x53
      , packageConfigExtraFrameworkDirs = x17
      , packageConfigExtraIncludeDirs = x18
      , packageConfigGHCiLib = x19
      , packageConfigSplitSections = x20
      , packageConfigSplitObjs = x20_1
      , packageConfigStripExes = x21
      , packageConfigStripLibs = x22
      , packageConfigTests = x23
      , packageConfigBenchmarks = x24
      , packageConfigCoverage = x25
      , packageConfigRelocatable = x26
      , packageConfigDebugInfo = x27
      , packageConfigDumpBuildInfo = x27_1
      , packageConfigRunTests = x28
      , packageConfigDocumentation = x29
      , packageConfigHaddockHoogle = x30
      , packageConfigHaddockHtml = x31
      , packageConfigHaddockHtmlLocation = x32
      , packageConfigHaddockForeignLibs = x33
      , packageConfigHaddockExecutables = x33_1
      , packageConfigHaddockTestSuites = x34
      , packageConfigHaddockBenchmarks = x35
      , packageConfigHaddockInternal = x36
      , packageConfigHaddockCss = x37
      , packageConfigHaddockLinkedSource = x38
      , packageConfigHaddockQuickJump = x43
      , packageConfigHaddockHscolourCss = x39
      , packageConfigHaddockContents = x40
      , packageConfigHaddockForHackage = x41
      , packageConfigHaddockIndex = x54
      , packageConfigHaddockBaseUrl = x55
      , packageConfigHaddockResourcesDir = x56
      , packageConfigHaddockOutputDir = x57
      , packageConfigTestHumanLog = x44
      , packageConfigTestMachineLog = x45
      , packageConfigTestShowDetails = x46
      , packageConfigTestKeepTix = x47
      , packageConfigTestWrapper = x48
      , packageConfigTestFailWhenNoTestSuites = x49
      , packageConfigTestTestOptions = x51
      , packageConfigBenchmarkOptions = x52
      } =
      [ PackageConfig
        { packageConfigProgramPaths = postShrink_Paths x00'
        , packageConfigProgramArgs = postShrink_Args x01'
        , packageConfigProgramPathExtra = x02'
        , packageConfigFlagAssignment = x03'
        , packageConfigVanillaLib = x04'
        , packageConfigSharedLib = x05'
        , packageConfigStaticLib = x42'
        , packageConfigDynExe = x06'
        , packageConfigFullyStaticExe = x50'
        , packageConfigProf = x07'
        , packageConfigProfLib = x08'
        , packageConfigProfExe = x09'
        , packageConfigProfDetail = x10'
        , packageConfigProfLibDetail = x11'
        , packageConfigConfigureArgs = map getNonEmpty x12'
        , packageConfigOptimization = x13'
        , packageConfigProgPrefix = x14'
        , packageConfigProgSuffix = x15'
        , packageConfigExtraLibDirs = map getNonEmpty x16'
        , packageConfigExtraLibDirsStatic = map getNonEmpty x53'
        , packageConfigExtraFrameworkDirs = map getNonEmpty x17'
        , packageConfigExtraIncludeDirs = map getNonEmpty x18'
        , packageConfigGHCiLib = x19'
        , packageConfigSplitSections = x20'
        , packageConfigSplitObjs = x20_1'
        , packageConfigStripExes = x21'
        , packageConfigStripLibs = x22'
        , packageConfigTests = x23'
        , packageConfigBenchmarks = x24'
        , packageConfigCoverage = x25'
        , packageConfigRelocatable = x26'
        , packageConfigDebugInfo = x27'
        , packageConfigDumpBuildInfo = x27_1'
        , packageConfigRunTests = x28'
        , packageConfigDocumentation = x29'
        , packageConfigHaddockHoogle = x30'
        , packageConfigHaddockHtml = x31'
        , packageConfigHaddockHtmlLocation = x32'
        , packageConfigHaddockForeignLibs = x33'
        , packageConfigHaddockExecutables = x33_1'
        , packageConfigHaddockTestSuites = x34'
        , packageConfigHaddockBenchmarks = x35'
        , packageConfigHaddockInternal = x36'
        , packageConfigHaddockCss = fmap getNonEmpty x37'
        , packageConfigHaddockLinkedSource = x38'
        , packageConfigHaddockQuickJump = x43'
        , packageConfigHaddockHscolourCss = fmap getNonEmpty x39'
        , packageConfigHaddockContents = x40'
        , packageConfigHaddockForHackage = x41'
        , packageConfigHaddockIndex = x54'
        , packageConfigHaddockBaseUrl = x55'
        , packageConfigHaddockResourcesDir = x56'
        , packageConfigHaddockOutputDir = x57'
        , packageConfigTestHumanLog = x44'
        , packageConfigTestMachineLog = x45'
        , packageConfigTestShowDetails = x46'
        , packageConfigTestKeepTix = x47'
        , packageConfigTestWrapper = x48'
        , packageConfigTestFailWhenNoTestSuites = x49'
        , packageConfigTestTestOptions = x51'
        , packageConfigBenchmarkOptions = x52'
        }
      | ( ( (x00', x01', x02', x03', x04')
            , (x05', x42', x06', x50', x07', x08', x09')
            , (x10', x11', x12', x13', x14')
            , (x15', x16', x53', x17', x18', x19')
            )
          , ( (x20', x20_1', x21', x22', x23', x24')
              , (x25', x26', x27', x27_1', x28', x29')
              , (x30', x31', x32', (x33', x33_1'), x34')
              , (x35', x36', x37', x38', x43', x39')
              , (x40', x41')
              , (x44', x45', x46', x47', x48', x49', x51', x52', x54', x55')
              , x56'
              , x57'
              )
          ) <-
          shrink
            (
              ( (preShrink_Paths x00, preShrink_Args x01, x02, x03, x04)
              , (x05, x42, x06, x50, x07, x08, x09)
              , (x10, x11, map NonEmpty x12, x13, x14)
              ,
                ( x15
                , map NonEmpty x16
                , map NonEmpty x53
                , map NonEmpty x17
                , map NonEmpty x18
                , x19
                )
              )
            ,
              ( (x20, x20_1, x21, x22, x23, x24)
              , (x25, x26, x27, x27_1, x28, x29)
              , (x30, x31, x32, (x33, x33_1), x34)
              , (x35, x36, fmap NonEmpty x37, x38, x43, fmap NonEmpty x39)
              , (x40, x41)
              , (x44, x45, x46, x47, x48, x49, x51, x52, x54, x55)
              , x56
              , x57
              )
            )
      ]
      where
        preShrink_Paths =
          Map.map NonEmpty
            . Map.mapKeys NoShrink
            . getMapLast
        postShrink_Paths =
          MapLast
            . Map.map getNonEmpty
            . Map.mapKeys getNoShrink
        preShrink_Args =
          Map.map (NonEmpty . map NonEmpty)
            . Map.mapKeys NoShrink
            . getMapMappend
        postShrink_Args =
          MapMappend
            . Map.map (map getNonEmpty . getNonEmpty)
            . Map.mapKeys getNoShrink

instance f ~ [] => Arbitrary (SourceRepositoryPackage f) where
  arbitrary =
    SourceRepositoryPackage
      <$> arbitrary
      <*> (getShortToken <$> arbitrary)
      <*> (fmap getShortToken <$> arbitrary)
      <*> (fmap getShortToken <$> arbitrary)
      <*> (fmap getShortToken <$> shortListOf 3 arbitrary)
      <*> (fmap getShortToken <$> shortListOf 3 arbitrary)

  shrink SourceRepositoryPackage{..} =
    runShrinker $
      pure SourceRepositoryPackage
        <*> shrinker srpType
        <*> shrinkerAla ShortToken srpLocation
        <*> shrinkerAla (fmap ShortToken) srpTag
        <*> shrinkerAla (fmap ShortToken) srpBranch
        <*> shrinkerAla (fmap ShortToken) srpSubdir
        <*> shrinkerAla (fmap ShortToken) srpCommand

instance Arbitrary RemoteRepo where
  arbitrary =
    RemoteRepo
      <$> arbitrary
      <*> arbitrary -- URI
      <*> arbitrary
      <*> listOf arbitraryRootKey
      <*> fmap getNonNegative arbitrary
      <*> pure False
    where
      arbitraryRootKey =
        shortListOf1
          5
          ( oneof
              [ choose ('0', '9')
              , choose ('a', 'f')
              ]
          )

instance Arbitrary LocalRepo where
  arbitrary =
    LocalRepo
      <$> arbitrary
      <*> elements ["/tmp/foo", "/tmp/bar"] -- TODO: generate valid absolute paths
      <*> arbitrary

instance Arbitrary PreSolver where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary ReorderGoals where
  arbitrary = ReorderGoals <$> arbitrary

instance Arbitrary CountConflicts where
  arbitrary = CountConflicts <$> arbitrary

instance Arbitrary FineGrainedConflicts where
  arbitrary = FineGrainedConflicts <$> arbitrary

instance Arbitrary MinimizeConflictSet where
  arbitrary = MinimizeConflictSet <$> arbitrary

instance Arbitrary IndependentGoals where
  arbitrary = IndependentGoals <$> arbitrary

instance Arbitrary PreferOldest where
  arbitrary = PreferOldest <$> arbitrary

instance Arbitrary StrongFlags where
  arbitrary = StrongFlags <$> arbitrary

instance Arbitrary AllowBootLibInstalls where
  arbitrary = AllowBootLibInstalls <$> arbitrary

instance Arbitrary OnlyConstrained where
  arbitrary =
    oneof
      [ pure OnlyConstrainedAll
      , pure OnlyConstrainedNone
      ]
