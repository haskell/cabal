{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Client.ProjectConfig (tests) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
import Control.Applicative
#endif
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

import Distribution.Deprecated.ParseUtils
import Distribution.Deprecated.Text as Text
import qualified Distribution.Deprecated.ReadP as Parse

import Distribution.Package
import Distribution.PackageDescription hiding (Flag)
import Distribution.Compiler
import Distribution.Version
import Distribution.Simple.Compiler
import Distribution.Simple.Setup
import Distribution.Simple.InstallDirs
import Distribution.Simple.Utils
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Db
import Distribution.Types.PackageVersionConstraint

import Distribution.Client.Types
import Distribution.Client.CmdInstall.ClientInstallFlags
import Distribution.Client.InstallSymlink
import Distribution.Client.Dependency.Types
import Distribution.Client.BuildReports.Types
import Distribution.Client.Targets
import Distribution.Utils.NubList
import Network.URI

import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.Settings

import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Legacy

import UnitTests.Distribution.Client.ArbitraryInstances
import UnitTests.Distribution.Client.TreeDiffInstances ()

import Data.TreeDiff.Class
import Data.TreeDiff.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testGroup "ProjectConfig <-> LegacyProjectConfig round trip" $
    [ testProperty "packages"  prop_roundtrip_legacytypes_packages
    , testProperty "buildonly" prop_roundtrip_legacytypes_buildonly
    , testProperty "specific"  prop_roundtrip_legacytypes_specific
    ] ++
    -- a couple tests seem to trigger a RTS fault in ghc-7.6 and older
    -- unclear why as of yet
    concat
    [ [ testProperty "shared"    prop_roundtrip_legacytypes_shared
      , testProperty "local"     prop_roundtrip_legacytypes_local
      , testProperty "all"       prop_roundtrip_legacytypes_all
      ]
    | not usingGhc76orOlder
    ]

  , testGroup "individual parser tests"
    [ testProperty "package location"  prop_parsePackageLocationTokenQ
    , testProperty "RelaxedDep"        prop_roundtrip_printparse_RelaxedDep
    , testProperty "RelaxDeps"         prop_roundtrip_printparse_RelaxDeps
    , testProperty "RelaxDeps'"        prop_roundtrip_printparse_RelaxDeps'
    ]

  , testGroup "ProjectConfig printing/parsing round trip"
    [ testProperty "packages"  prop_roundtrip_printparse_packages
    , testProperty "buildonly" prop_roundtrip_printparse_buildonly
    , testProperty "shared"    prop_roundtrip_printparse_shared
    , testProperty "local"     prop_roundtrip_printparse_local
    , testProperty "specific"  prop_roundtrip_printparse_specific
    , testProperty "all"       prop_roundtrip_printparse_all
    ]
  ]
  where
    usingGhc76orOlder =
      case buildCompilerId of
        CompilerId GHC v -> v < mkVersion [7,7]
        _                -> False


------------------------------------------------
-- Round trip: conversion to/from legacy types
--

roundtrip :: (Eq a, ToExpr a) => (a -> b) -> (b -> a) -> a -> Property
roundtrip f f_inv x =
    let y = f x
    in f_inv y `ediffEq` x -- no counterexample with y, as they not have ToExpr

roundtrip_legacytypes :: ProjectConfig -> Property
roundtrip_legacytypes =
    roundtrip convertToLegacyProjectConfig
              convertLegacyProjectConfig


prop_roundtrip_legacytypes_all :: ProjectConfig -> Property
prop_roundtrip_legacytypes_all config =
    roundtrip_legacytypes
      config {
        projectConfigProvenance = mempty
      }

prop_roundtrip_legacytypes_packages :: ProjectConfig -> Property
prop_roundtrip_legacytypes_packages config =
    roundtrip_legacytypes
      config {
        projectConfigBuildOnly       = mempty,
        projectConfigShared          = mempty,
        projectConfigProvenance      = mempty,
        projectConfigLocalPackages   = mempty,
        projectConfigSpecificPackage = mempty
      }

prop_roundtrip_legacytypes_buildonly :: ProjectConfigBuildOnly -> Property
prop_roundtrip_legacytypes_buildonly config =
    roundtrip_legacytypes
      mempty { projectConfigBuildOnly = config }

prop_roundtrip_legacytypes_shared :: ProjectConfigShared -> Property
prop_roundtrip_legacytypes_shared config =
    roundtrip_legacytypes
      mempty { projectConfigShared = config }

prop_roundtrip_legacytypes_local :: PackageConfig -> Property
prop_roundtrip_legacytypes_local config =
    roundtrip_legacytypes
      mempty { projectConfigLocalPackages = config }

prop_roundtrip_legacytypes_specific :: Map PackageName PackageConfig -> Property
prop_roundtrip_legacytypes_specific config =
    roundtrip_legacytypes
      mempty { projectConfigSpecificPackage = MapMappend config }


--------------------------------------------
-- Round trip: printing and parsing config
--

roundtrip_printparse :: ProjectConfig -> Property
roundtrip_printparse config =
    case (fmap convertLegacyProjectConfig
        . parseLegacyProjectConfig
        . showLegacyProjectConfig
        . convertToLegacyProjectConfig)
          config of
      ParseOk _ x     -> x `ediffEq` config { projectConfigProvenance = mempty }
      ParseFailed err -> counterexample (show err) False


prop_roundtrip_printparse_all :: ProjectConfig -> Property
prop_roundtrip_printparse_all config =
    roundtrip_printparse config {
      projectConfigBuildOnly =
        hackProjectConfigBuildOnly (projectConfigBuildOnly config),

      projectConfigShared =
        hackProjectConfigShared (projectConfigShared config)
    }

prop_roundtrip_printparse_packages :: [PackageLocationString]
                                   -> [PackageLocationString]
                                   -> [SourceRepo]
                                   -> [PackageVersionConstraint]
                                   -> Property
prop_roundtrip_printparse_packages pkglocstrs1 pkglocstrs2 repos named =
    roundtrip_printparse
      mempty {
        projectPackages         = map getPackageLocationString pkglocstrs1,
        projectPackagesOptional = map getPackageLocationString pkglocstrs2,
        projectPackagesRepo     = repos,
        projectPackagesNamed    = named
      }

prop_roundtrip_printparse_buildonly :: ProjectConfigBuildOnly -> Property
prop_roundtrip_printparse_buildonly config =
    roundtrip_printparse
      mempty {
        projectConfigBuildOnly = hackProjectConfigBuildOnly config
      }

hackProjectConfigBuildOnly :: ProjectConfigBuildOnly -> ProjectConfigBuildOnly
hackProjectConfigBuildOnly config =
    config {
      -- These two fields are only command line transitory things, not
      -- something to be recorded persistently in a config file
      projectConfigOnlyDeps = mempty,
      projectConfigDryRun   = mempty
    }

prop_roundtrip_printparse_shared :: ProjectConfigShared -> Property
prop_roundtrip_printparse_shared config =
    roundtrip_printparse
      mempty {
        projectConfigShared = hackProjectConfigShared config
      }

hackProjectConfigShared :: ProjectConfigShared -> ProjectConfigShared
hackProjectConfigShared config =
    config {
      projectConfigProjectFile = mempty, -- not present within project files
      projectConfigConfigFile  = mempty, -- ditto
      projectConfigConstraints =
      --TODO: [required eventually] parse ambiguity in constraint
      -- "pkgname -any" as either any version or disabled flag "any".
        let ambiguous (UserConstraint _ (PackagePropertyFlags flags), _) =
              (not . null) [ () | (name, False) <- unFlagAssignment flags
                                , "any" `isPrefixOf` unFlagName name ]
            ambiguous _ = False
         in filter (not . ambiguous) (projectConfigConstraints config)
    }


prop_roundtrip_printparse_local :: PackageConfig -> Property
prop_roundtrip_printparse_local config =
    roundtrip_printparse
      mempty {
        projectConfigLocalPackages = config
      }

prop_roundtrip_printparse_specific :: Map PackageName (NonMEmpty PackageConfig)
                                   -> Property
prop_roundtrip_printparse_specific config =
    roundtrip_printparse
      mempty {
        projectConfigSpecificPackage = MapMappend (fmap getNonMEmpty config)
      }


----------------------------
-- Individual Parser tests
--

-- | Helper to parse a given string
--
-- Succeeds only if there is a unique complete parse
runReadP :: Parse.ReadP a a -> String -> Maybe a
runReadP parser s = case [ x | (x,"") <- Parse.readP_to_S parser s ] of
                      [x'] -> Just x'
                      _    -> Nothing

prop_parsePackageLocationTokenQ :: PackageLocationString -> Bool
prop_parsePackageLocationTokenQ (PackageLocationString str) =
    runReadP parsePackageLocationTokenQ (renderPackageLocationToken str) == Just str

prop_roundtrip_printparse_RelaxedDep :: RelaxedDep -> Bool
prop_roundtrip_printparse_RelaxedDep rdep =
    runReadP Text.parse (Text.display rdep) == Just rdep

prop_roundtrip_printparse_RelaxDeps :: RelaxDeps -> Property
prop_roundtrip_printparse_RelaxDeps rdep =
    counterexample (Text.display rdep) $
    runReadP Text.parse (Text.display rdep) `ediffEq` Just rdep

prop_roundtrip_printparse_RelaxDeps' :: RelaxDeps -> Property
prop_roundtrip_printparse_RelaxDeps' rdep =
    counterexample rdep' $
    runReadP Text.parse rdep' `ediffEq` Just rdep
  where
    rdep' = go (Text.display rdep)

    -- replace 'all' tokens by '*'
    go :: String -> String
    go [] = []
    go "all" = "*"
    go ('a':'l':'l':c:rest) | c `elem` ":," = '*' : go (c:rest)
    go rest = let (x,y) = break (`elem` ":,") rest
                  (x',y') = span (`elem` ":,^") y
              in x++x'++go y'

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
        <*> (MapMappend . fmap getNonMEmpty . Map.fromList
               <$> shortListOf 3 arbitrary)
        -- package entries with no content are equivalent to
        -- the entry not existing at all, so exclude empty

    shrink ProjectConfig { projectPackages = x0
                         , projectPackagesOptional = x1
                         , projectPackagesRepo = x2
                         , projectPackagesNamed = x3
                         , projectConfigBuildOnly = x4
                         , projectConfigShared = x5
                         , projectConfigProvenance = x6
                         , projectConfigLocalPackages = x7
                         , projectConfigSpecificPackage = x8
                         , projectConfigAllPackages = x9 } =
      [ ProjectConfig { projectPackages = x0'
                      , projectPackagesOptional = x1'
                      , projectPackagesRepo = x2'
                      , projectPackagesNamed = x3'
                      , projectConfigBuildOnly = x4'
                      , projectConfigShared = x5'
                      , projectConfigProvenance = x6'
                      , projectConfigLocalPackages = x7'
                      , projectConfigSpecificPackage = (MapMappend
                                                         (fmap getNonMEmpty x8'))
                      , projectConfigAllPackages = x9' }
      | ((x0', x1', x2', x3'), (x4', x5', x6', x7', x8', x9'))
          <- shrink ((x0, x1, x2, x3),
                      (x4, x5, x6, x7, fmap NonMEmpty (getMapMappend x8), x9))
      ]

newtype PackageLocationString
      = PackageLocationString { getPackageLocationString :: String }
  deriving Show

instance Arbitrary PackageLocationString where
  arbitrary =
    PackageLocationString <$>
    oneof
      [ show . getNonEmpty <$> (arbitrary :: Gen (NonEmptyList String))
      , arbitraryGlobLikeStr
      , show <$> (arbitrary :: Gen URI)
      ]

arbitraryGlobLikeStr :: Gen String
arbitraryGlobLikeStr = outerTerm
  where
    outerTerm  = concat <$> shortListOf1 4
                  (frequency [ (2, token)
                             , (1, braces <$> innerTerm) ])
    innerTerm  = intercalate "," <$> shortListOf1 3
                  (frequency [ (3, token)
                             , (1, braces <$> innerTerm) ])
    token      = shortListOf1 4 (elements (['#'..'~'] \\ "{,}"))
    braces s   = "{" ++ s ++ "}"


instance Arbitrary OverwritePolicy where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary InstallMethod where
    arbitrary = arbitraryBoundedEnum

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
        <*> (toNubList <$> shortListOf 2 arbitrary)             --  4
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (fmap getShortToken <$> arbitrary)                  --  8
        <*> arbitrary
        <*> arbitraryNumJobs
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

    shrink ProjectConfigBuildOnly { projectConfigVerbosity = x00
                                  , projectConfigDryRun = x01
                                  , projectConfigOnlyDeps = x02
                                  , projectConfigSummaryFile = x03
                                  , projectConfigLogFile = x04
                                  , projectConfigBuildReports = x05
                                  , projectConfigReportPlanningFailure = x06
                                  , projectConfigSymlinkBinDir = x07
                                  , projectConfigOneShot = x08
                                  , projectConfigNumJobs = x09
                                  , projectConfigKeepGoing = x10
                                  , projectConfigOfflineMode = x11
                                  , projectConfigKeepTempFiles = x12
                                  , projectConfigHttpTransport = x13
                                  , projectConfigIgnoreExpiry = x14
                                  , projectConfigCacheDir = x15
                                  , projectConfigLogsDir = x16
                                  , projectConfigClientInstallFlags = x17 } =
      [ ProjectConfigBuildOnly { projectConfigVerbosity = x00'
                               , projectConfigDryRun = x01'
                               , projectConfigOnlyDeps = x02'
                               , projectConfigSummaryFile = x03'
                               , projectConfigLogFile = x04'
                               , projectConfigBuildReports = x05'
                               , projectConfigReportPlanningFailure = x06'
                               , projectConfigSymlinkBinDir = x07'
                               , projectConfigOneShot = x08'
                               , projectConfigNumJobs = postShrink_NumJobs x09'
                               , projectConfigKeepGoing = x10'
                               , projectConfigOfflineMode = x11'
                               , projectConfigKeepTempFiles = x12'
                               , projectConfigHttpTransport = x13
                               , projectConfigIgnoreExpiry = x14'
                               , projectConfigCacheDir = x15
                               , projectConfigLogsDir = x16
                               , projectConfigClientInstallFlags = x17' }
      | ((x00', x01', x02', x03', x04'),
         (x05', x06', x07', x08', x09'),
         (x10', x11', x12',       x14'),
         (            x17'            ))
          <- shrink
               ((x00, x01, x02, x03, x04),
                (x05, x06, x07, x08, preShrink_NumJobs x09),
                (x10, x11, x12,      x14),
                (          x17          ))
      ]
      where
        preShrink_NumJobs  = fmap (fmap Positive)
        postShrink_NumJobs = fmap (fmap getPositive)

instance Arbitrary ProjectConfigShared where
    arbitrary =
      ProjectConfigShared
        <$> arbitraryFlag arbitraryShortToken
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitrary
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitrary
        <*> arbitrary
        <*> (toNubList <$> listOf arbitraryShortToken)
        <*> arbitrary
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitraryConstraints
        <*> shortListOf 2 arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (toNubList <$> listOf arbitraryShortToken)
      where
        arbitraryConstraints :: Gen [(UserConstraint, ConstraintSource)]
        arbitraryConstraints =
            fmap (\uc -> (uc, projectConfigConstraintSource)) <$> arbitrary

    shrink ProjectConfigShared { projectConfigDistDir = x00
                               , projectConfigProjectFile = x01
                               , projectConfigHcFlavor = x02
                               , projectConfigHcPath = x03
                               , projectConfigHcPkg = x04
                               , projectConfigHaddockIndex = x05
                               , projectConfigRemoteRepos = x06
                               , projectConfigLocalRepos = x07
                               , projectConfigIndexState = x08
                               , projectConfigConstraints = x09
                               , projectConfigPreferences = x10
                               , projectConfigCabalVersion = x11
                               , projectConfigSolver = x12
                               , projectConfigAllowOlder = x13
                               , projectConfigAllowNewer = x14
                               , projectConfigWriteGhcEnvironmentFilesPolicy = x15
                               , projectConfigMaxBackjumps = x16
                               , projectConfigReorderGoals = x17
                               , projectConfigCountConflicts = x18
                               , projectConfigMinimizeConflictSet = x19
                               , projectConfigStrongFlags = x20
                               , projectConfigAllowBootLibInstalls = x21
                               , projectConfigOnlyConstrained = x22
                               , projectConfigPerComponent = x23
                               , projectConfigIndependentGoals = x24
                               , projectConfigConfigFile = x25
                               , projectConfigProgPathExtra = x26
                               , projectConfigStoreDir = x27 } =
      [ ProjectConfigShared { projectConfigDistDir = x00'
                            , projectConfigProjectFile = x01'
                            , projectConfigHcFlavor = x02'
                            , projectConfigHcPath = fmap getNonEmpty x03'
                            , projectConfigHcPkg = fmap getNonEmpty x04'
                            , projectConfigHaddockIndex = x05'
                            , projectConfigRemoteRepos = x06'
                            , projectConfigLocalRepos = x07'
                            , projectConfigIndexState = x08'
                            , projectConfigConstraints = postShrink_Constraints x09'
                            , projectConfigPreferences = x10'
                            , projectConfigCabalVersion = x11'
                            , projectConfigSolver = x12'
                            , projectConfigAllowOlder = x13'
                            , projectConfigAllowNewer = x14'
                            , projectConfigWriteGhcEnvironmentFilesPolicy = x15'
                            , projectConfigMaxBackjumps = x16'
                            , projectConfigReorderGoals = x17'
                            , projectConfigCountConflicts = x18'
                            , projectConfigMinimizeConflictSet = x19'
                            , projectConfigStrongFlags = x20'
                            , projectConfigAllowBootLibInstalls = x21'
                            , projectConfigOnlyConstrained = x22'
                            , projectConfigPerComponent = x23'
                            , projectConfigIndependentGoals = x24'
                            , projectConfigConfigFile = x25'
                            , projectConfigProgPathExtra = x26'
                            , projectConfigStoreDir = x27' }
      | ((x00', x01', x02', x03', x04'),
         (x05', x06', x07', x08', x09'),
         (x10', x11', x12', x13', x14', x15'),
         (x16', x17', x18', x19', x20', x21'),
          x22', x23', x24', x25', x26', x27')
          <- shrink
               ((x00, x01, x02, fmap NonEmpty x03, fmap NonEmpty x04),
                (x05, x06, x07, x08, preShrink_Constraints x09),
                (x10, x11, x12, x13, x14, x15),
                (x16, x17, x18, x19, x20, x21),
                 x22, x23, x24, x25, x26, x27)
      ]
      where
        preShrink_Constraints  = map fst
        postShrink_Constraints = map (\uc -> (uc, projectConfigConstraintSource))

projectConfigConstraintSource :: ConstraintSource
projectConfigConstraintSource =
    ConstraintSourceProjectConfig "TODO"

instance Arbitrary ProjectConfigProvenance where
    arbitrary = elements [Implicit, Explicit "cabal.project"]

instance Arbitrary FlagAssignment where
    arbitrary = mkFlagAssignment <$> arbitrary

instance Arbitrary PackageConfig where
    arbitrary =
      PackageConfig
        <$> (MapLast . Map.fromList <$> shortListOf 10
              ((,) <$> arbitraryProgramName
                   <*> arbitraryShortToken))
        <*> (MapMappend . Map.fromList <$> shortListOf 10
              ((,) <$> arbitraryProgramName
                   <*> listOf arbitraryShortToken))
        <*> (toNubList <$> listOf arbitraryShortToken)
        <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> shortListOf 5 arbitraryShortToken
        <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> shortListOf 5 arbitraryShortToken
        <*> shortListOf 5 arbitraryShortToken
        <*> shortListOf 5 arbitraryShortToken
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitrary
        <*> arbitrary
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitrary
        <*> shortListOf 5 arbitrary
      where
        arbitraryProgramName :: Gen String
        arbitraryProgramName =
          elements [ programName prog
                   | (prog, _) <- knownPrograms (defaultProgramDb) ]

    shrink PackageConfig { packageConfigProgramPaths = x00
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
                         , packageConfigTestHumanLog = x44
                         , packageConfigTestMachineLog = x45
                         , packageConfigTestShowDetails = x46
                         , packageConfigTestKeepTix = x47
                         , packageConfigTestWrapper = x48
                         , packageConfigTestFailWhenNoTestSuites = x49
                         , packageConfigTestTestOptions = x51 } =
      [ PackageConfig { packageConfigProgramPaths = postShrink_Paths x00'
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
                      , packageConfigTestHumanLog = x44'
                      , packageConfigTestMachineLog = x45'
                      , packageConfigTestShowDetails = x46'
                      , packageConfigTestKeepTix = x47'
                      , packageConfigTestWrapper = x48'
                      , packageConfigTestFailWhenNoTestSuites = x49'
                      , packageConfigTestTestOptions = x51' }
      |  (((x00', x01', x02', x03', x04'),
          (x05', x42', x06', x50', x07', x08', x09'),
          (x10', x11', x12', x13', x14'),
          (x15', x16', x17', x18', x19')),
         ((x20', x20_1', x21', x22', x23', x24'),
          (x25', x26', x27', x28', x29'),
          (x30', x31', x32', (x33', x33_1'), x34'),
          (x35', x36', x37', x38', x43', x39'),
          (x40', x41'),
          (x44', x45', x46', x47', x48', x49', x51')))
          <- shrink
             (((preShrink_Paths x00, preShrink_Args x01, x02, x03, x04),
                (x05, x42, x06, x50, x07, x08, x09),
                (x10, x11, map NonEmpty x12, x13, x14),
                (x15, map NonEmpty x16,
                  map NonEmpty x17,
                  map NonEmpty x18,
                  x19)),
               ((x20, x20_1, x21, x22, x23, x24),
                 (x25, x26, x27, x28, x29),
                 (x30, x31, x32, (x33, x33_1), x34),
                 (x35, x36, fmap NonEmpty x37, x38, x43, fmap NonEmpty x39),
                 (x40, x41),
                 (x44, x45, x46, x47, x48, x49, x51)))
      ]
      where
        preShrink_Paths  = Map.map NonEmpty
                         . Map.mapKeys NoShrink
                         . getMapLast
        postShrink_Paths = MapLast
                         . Map.map getNonEmpty
                         . Map.mapKeys getNoShrink
        preShrink_Args   = Map.map (NonEmpty . map NonEmpty)
                         . Map.mapKeys NoShrink
                         . getMapMappend
        postShrink_Args  = MapMappend
                         . Map.map (map getNonEmpty . getNonEmpty)
                         . Map.mapKeys getNoShrink

instance Arbitrary HaddockTarget where
    arbitrary = elements [ForHackage, ForDevelopment]

instance Arbitrary TestShowDetails where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary SourceRepo where
    arbitrary = (SourceRepo kind
                           <$> arbitrary
                           <*> (fmap getShortToken <$> arbitrary)
                           <*> (fmap getShortToken <$> arbitrary)
                           <*> (fmap getShortToken <$> arbitrary)
                           <*> (fmap getShortToken <$> arbitrary)
                           <*> (fmap getShortToken <$> arbitrary))
                `suchThat` (/= emptySourceRepo kind)
      where
        kind = RepoKindUnknown "unused"

    shrink (SourceRepo _ x1 x2 x3 x4 x5 x6) =
      [ repo
      | ((x1', x2', x3'), (x4', x5', x6'))
          <- shrink ((x1,
                      fmap ShortToken x2,
                      fmap ShortToken x3),
                     (fmap ShortToken x4,
                      fmap ShortToken x5,
                      fmap ShortToken x6))
      , let repo = SourceRepo RepoThis x1'
                              (fmap getShortToken x2')
                              (fmap getShortToken x3')
                              (fmap getShortToken x4')
                              (fmap getShortToken x5')
                              (fmap getShortToken x6')
      , repo /= emptySourceRepo RepoThis
      ]

instance Arbitrary RepoType where
    arbitrary = elements knownRepoTypes

instance Arbitrary ReportLevel where
    arbitrary = elements [NoReports .. DetailedReports]

instance Arbitrary CompilerFlavor where
    arbitrary = elements knownCompilerFlavors

instance Arbitrary a => Arbitrary (InstallDirs a) where
    arbitrary =
      InstallDirs
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary --  4
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary --  8
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 12
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 16

instance Arbitrary PackageDB where
    arbitrary = oneof [ pure GlobalPackageDB
                      , pure UserPackageDB
                      , SpecificPackageDB . getShortToken <$> arbitrary
                      ]

instance Arbitrary RemoteRepo where
    arbitrary =
      RemoteRepo
        <$> arbitraryShortToken `suchThat` (not . (":" `isPrefixOf`))
        <*> arbitrary  -- URI
        <*> arbitrary
        <*> listOf arbitraryRootKey
        <*> (fmap getNonNegative arbitrary)
        <*> pure False
      where
        arbitraryRootKey =
          shortListOf1 5 (oneof [ choose ('0', '9')
                                , choose ('a', 'f') ])

instance Arbitrary UserConstraintScope where
    arbitrary = oneof [ UserQualified <$> arbitrary <*> arbitrary
                      , UserAnySetupQualifier <$> arbitrary
                      , UserAnyQualifier <$> arbitrary
                      ]

instance Arbitrary UserQualifier where
    arbitrary = oneof [ pure UserQualToplevel
                      , UserQualSetup <$> arbitrary

                      -- -- TODO: Re-enable UserQualExe tests once we decide on a syntax.
                      -- , UserQualExe <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary UserConstraint where
    arbitrary = UserConstraint <$> arbitrary <*> arbitrary

instance Arbitrary PackageProperty where
    arbitrary = oneof [ PackagePropertyVersion <$> arbitrary
                      , pure PackagePropertyInstalled
                      , pure PackagePropertySource
                      , PackagePropertyFlags  . mkFlagAssignment <$> shortListOf1 3 arbitrary
                      , PackagePropertyStanzas . (\x->[x]) <$> arbitrary
                      ]

instance Arbitrary OptionalStanza where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary FlagName where
    arbitrary = mkFlagName <$> flagident
      where
        flagident   = lowercase <$> shortListOf1 5 (elements flagChars)
                      `suchThat` (("-" /=) . take 1)
        flagChars   = "-_" ++ ['a'..'z']

instance Arbitrary PreSolver where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary ReorderGoals where
    arbitrary = ReorderGoals <$> arbitrary

instance Arbitrary CountConflicts where
    arbitrary = CountConflicts <$> arbitrary

instance Arbitrary MinimizeConflictSet where
    arbitrary = MinimizeConflictSet <$> arbitrary

instance Arbitrary IndependentGoals where
    arbitrary = IndependentGoals <$> arbitrary

instance Arbitrary StrongFlags where
    arbitrary = StrongFlags <$> arbitrary

instance Arbitrary AllowBootLibInstalls where
    arbitrary = AllowBootLibInstalls <$> arbitrary

instance Arbitrary OnlyConstrained where
    arbitrary = oneof [ pure OnlyConstrainedAll
                      , pure OnlyConstrainedNone
                      ]

instance Arbitrary AllowNewer where
    arbitrary = AllowNewer <$> arbitrary

instance Arbitrary AllowOlder where
    arbitrary = AllowOlder <$> arbitrary

instance Arbitrary RelaxDeps where
    arbitrary = oneof [ pure mempty
                      , RelaxDepsSome <$> shortListOf1 3 arbitrary
                      , pure RelaxDepsAll
                      ]

instance Arbitrary RelaxDepMod where
    arbitrary = elements [RelaxDepModNone, RelaxDepModCaret]

instance Arbitrary RelaxDepScope where
    arbitrary = oneof [ pure RelaxDepScopeAll
                      , RelaxDepScopePackage <$> arbitrary
                      , RelaxDepScopePackageId <$> (PackageIdentifier <$> arbitrary <*> arbitrary)
                      ]

instance Arbitrary RelaxDepSubject where
    arbitrary = oneof [ pure RelaxDepSubjectAll
                      , RelaxDepSubjectPkg <$> arbitrary
                      ]

instance Arbitrary RelaxedDep where
    arbitrary = RelaxedDep <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ProfDetailLevel where
    arbitrary = elements [ d | (_,_,d) <- knownProfDetailLevels ]

instance Arbitrary OptimisationLevel where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary DebugInfoLevel where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary URI where
    arbitrary =
      URI <$> elements ["file:", "http:", "https:"]
          <*> (Just <$> arbitrary)
          <*> (('/':) <$> arbitraryURIToken)
          <*> (('?':) <$> arbitraryURIToken)
          <*> pure ""

instance Arbitrary URIAuth where
    arbitrary =
      URIAuth <$> pure ""   -- no password as this does not roundtrip
              <*> arbitraryURIToken
              <*> arbitraryURIPort

arbitraryURIToken :: Gen String
arbitraryURIToken =
    shortListOf1 6 (elements (filter isUnreserved ['\0'..'\255']))

arbitraryURIPort :: Gen String
arbitraryURIPort =
    oneof [ pure "", (':':) <$> shortListOf1 4 (choose ('0','9')) ]
