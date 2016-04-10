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

import Distribution.Package
import Distribution.PackageDescription hiding (Flag)
import Distribution.Compiler
import Distribution.Version
import Distribution.ParseUtils
import Distribution.Simple.Compiler
import Distribution.Simple.Setup
import Distribution.Simple.InstallDirs
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Simple.Utils
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Db

import Distribution.Client.Types
import Distribution.Client.Dependency.Types
import Distribution.Client.BuildReports.Types
import Distribution.Client.Targets
import Distribution.Utils.NubList
import Network.URI

import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Legacy

import UnitTests.Distribution.Client.ArbitraryInstances

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
        CompilerId GHC v -> v < Version [7,7] []
        _                -> False


------------------------------------------------
-- Round trip: conversion to/from legacy types
--

roundtrip :: Eq a => (a -> b) -> (b -> a) -> a -> Bool
roundtrip f f_inv x =
    (f_inv . f) x == x

roundtrip_legacytypes :: ProjectConfig -> Bool
roundtrip_legacytypes =
    roundtrip convertToLegacyProjectConfig
              convertLegacyProjectConfig


prop_roundtrip_legacytypes_all :: ProjectConfig -> Bool
prop_roundtrip_legacytypes_all =
    roundtrip_legacytypes

prop_roundtrip_legacytypes_packages :: ProjectConfig -> Bool
prop_roundtrip_legacytypes_packages config =
    roundtrip_legacytypes
      config {
        projectConfigBuildOnly       = mempty,
        projectConfigShared          = mempty,
        projectConfigLocalPackages   = mempty,
        projectConfigSpecificPackage = mempty
      }

prop_roundtrip_legacytypes_buildonly :: ProjectConfigBuildOnly -> Bool
prop_roundtrip_legacytypes_buildonly config =
    roundtrip_legacytypes
      mempty { projectConfigBuildOnly = config }

prop_roundtrip_legacytypes_shared :: ProjectConfigShared -> Bool
prop_roundtrip_legacytypes_shared config =
    roundtrip_legacytypes
      mempty { projectConfigShared = config }

prop_roundtrip_legacytypes_local :: PackageConfig -> Bool
prop_roundtrip_legacytypes_local config =
    roundtrip_legacytypes
      mempty { projectConfigLocalPackages = config }

prop_roundtrip_legacytypes_specific :: Map PackageName PackageConfig -> Bool
prop_roundtrip_legacytypes_specific config =
    roundtrip_legacytypes
      mempty { projectConfigSpecificPackage = MapMappend config }


--------------------------------------------
-- Round trip: printing and parsing config
--

roundtrip_printparse :: ProjectConfig -> Bool
roundtrip_printparse config =
    case (fmap convertLegacyProjectConfig
        . parseLegacyProjectConfig
        . showLegacyProjectConfig
        . convertToLegacyProjectConfig)
          config of
      ParseOk _ x -> x == config
      _           -> False


prop_roundtrip_printparse_all :: ProjectConfig -> Bool
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
                                   -> [Dependency]
                                   -> Bool
prop_roundtrip_printparse_packages pkglocstrs1 pkglocstrs2 repos named =
    roundtrip_printparse
      mempty {
        projectPackages         = map getPackageLocationString pkglocstrs1,
        projectPackagesOptional = map getPackageLocationString pkglocstrs2,
        projectPackagesRepo     = repos,
        projectPackagesNamed    = named
      }

prop_roundtrip_printparse_buildonly :: ProjectConfigBuildOnly -> Bool
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

prop_roundtrip_printparse_shared :: ProjectConfigShared -> Bool
prop_roundtrip_printparse_shared config =
    roundtrip_printparse
      mempty {
        projectConfigShared = hackProjectConfigShared config
      }

hackProjectConfigShared :: ProjectConfigShared -> ProjectConfigShared
hackProjectConfigShared config =
    config {
      projectConfigConstraints =
      --TODO: [required eventually] parse ambiguity in constraint
      -- "pkgname -any" as either any version or disabled flag "any".
        let ambiguous ((UserConstraintFlags _pkg flags), _) =
              (not . null) [ () | (FlagName name, False) <- flags
                                , "any" `isPrefixOf` name ]
            ambiguous _ = False
         in filter (not . ambiguous) (projectConfigConstraints config)
    }


prop_roundtrip_printparse_local :: PackageConfig -> Bool
prop_roundtrip_printparse_local config =
    roundtrip_printparse
      mempty {
        projectConfigLocalPackages = config
      }

prop_roundtrip_printparse_specific :: Map PackageName (NonMEmpty PackageConfig)
                                   -> Bool
prop_roundtrip_printparse_specific config =
    roundtrip_printparse
      mempty {
        projectConfigSpecificPackage = MapMappend (fmap getNonMEmpty config)
      }


----------------------------
-- Individual Parser tests 
--

prop_parsePackageLocationTokenQ :: PackageLocationString -> Bool
prop_parsePackageLocationTokenQ (PackageLocationString str) =
    case [ x | (x,"") <- Parse.readP_to_S parsePackageLocationTokenQ
                                         (renderPackageLocationToken str) ] of
      [str'] -> str' == str
      _      -> False


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
        <*> arbitrary <*> arbitrary
        <*> arbitrary
        <*> (MapMappend . fmap getNonMEmpty . Map.fromList
               <$> shortListOf 3 arbitrary)
        -- package entries with no content are equivalent to
        -- the entry not existing at all, so exclude empty

    shrink (ProjectConfig x0 x1 x2 x3 x4 x5 x6 x7) =
      [ ProjectConfig x0' x1' x2' x3'
                      x4' x5' x6' (MapMappend (fmap getNonMEmpty x7'))
      | ((x0', x1', x2', x3'), (x4', x5', x6', x7'))
          <- shrink ((x0, x1, x2, x3),
                     (x4, x5, x6, fmap NonMEmpty (getMapMappend x7)))
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
        <*> arbitrary                                           -- 12
        <*> (fmap getShortToken <$> arbitrary)
        <*> arbitrary
        <*> (fmap getShortToken <$> arbitrary)
        <*> (fmap getShortToken <$> arbitrary)                  -- 16
        <*> (fmap getShortToken <$> arbitrary)
        <*> (fmap getShortToken <$> arbitrary)
      where
        arbitraryNumJobs = fmap (fmap getPositive) <$> arbitrary

    shrink (ProjectConfigBuildOnly
              x00 x01 x02 x03 x04 x05 x06 x07
              x08 x09 x10 x11 x12 x13 x14 x15
              x16 x17) =
      [ ProjectConfigBuildOnly
          x00' x01' x02' x03' x04'
          x05' x06' x07' x08' (postShrink_NumJobs x09')
          x10' x11' x12  x13' x14
          x15 x16 x17
      | ((x00', x01', x02', x03', x04'),
         (x05', x06', x07', x08', x09'),
         (x10', x11',       x13'))
          <- shrink
               ((x00, x01, x02, x03, x04),
                (x05, x06, x07, x08, preShrink_NumJobs x09),
                (x10, x11,      x13))
      ]
      where
        preShrink_NumJobs  = fmap (fmap Positive)
        postShrink_NumJobs = fmap (fmap getPositive)

instance Arbitrary ProjectConfigShared where
    arbitrary =
      ProjectConfigShared
        <$> arbitrary                                           --  4
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitrary
        <*> arbitrary
        <*> (toNubList <$> listOf arbitraryShortToken)
        <*> arbitraryConstraints
        <*> shortListOf 2 arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
      where
        arbitraryConstraints :: Gen [(UserConstraint, ConstraintSource)]
        arbitraryConstraints =
            map (\uc -> (uc, projectConfigConstraintSource)) <$> arbitrary

    shrink (ProjectConfigShared
              x00 x01 x02 x03 x04
              x05 x06 x07 x08 x09
              x10 x11 x12 x13) =
      [ ProjectConfigShared
          x00' (fmap getNonEmpty x01') (fmap getNonEmpty x02') x03' x04'
          x05' (postShrink_Constraints x06') x07' x08' x09'
          x10' x11' x12' x13'
      | ((x00', x01', x02', x03', x04'),
         (x05', x06', x07', x08', x09'),
         (x10', x11', x12', x13'))
          <- shrink
               ((x00, fmap NonEmpty x01, fmap NonEmpty x02, x03, x04),
                (x05, preShrink_Constraints x06, x07, x08, x09),
                (x10, x11, x12, x13))
      ]
      where
        preShrink_Constraints  = map fst
        postShrink_Constraints = map (\uc -> (uc, projectConfigConstraintSource))

projectConfigConstraintSource :: ConstraintSource
projectConfigConstraintSource = 
    ConstraintSourceProjectConfig "TODO"

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
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> shortListOf 5 arbitraryShortToken
        <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> shortListOf 5 arbitraryShortToken
        <*> shortListOf 5 arbitraryShortToken
        <*> shortListOf 5 arbitraryShortToken
        <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitrary
        <*> arbitraryFlag arbitraryShortToken
        <*> arbitrary
      where
        arbitraryProgramName :: Gen String
        arbitraryProgramName =
          elements [ programName prog
                   | (prog, _) <- knownPrograms (defaultProgramDb) ]

    shrink (PackageConfig
              x00 x01 x02 x03 x04
              x05 x06 x07 x08 x09
              x10 x11 x12 x13 x14
              x15 x16 x17 x18 x19
              x20 x21 x22 x23 x24
              x25 x26 x27 x28 x29
              x30 x31 x32 x33 x34
              x35 x36 x37 x38 x39
              x40) =
      [ PackageConfig
          (postShrink_Paths x00')
          (postShrink_Args  x01') x02' x03' x04'
          x05' x06' x07' x08' x09'
          x10' x11' (map getNonEmpty x12') x13' x14'
          x15' (map getNonEmpty x16')
               (map getNonEmpty x17')
               (map getNonEmpty x18')
                              x19'
          x20' x21' x22' x23' x24'
          x25' x26' x27' x28' x29'
          x30' x31' x32' x33' x34'
          x35' x36' (fmap getNonEmpty x37') x38'
                    (fmap getNonEmpty x39')
          x40'
      | (((x00', x01', x02', x03', x04'),
          (x05', x06', x07', x08', x09'),
          (x10', x11', x12', x13', x14'),
          (x15', x16', x17', x18', x19')),
         ((x20', x21', x22', x23', x24'),
          (x25', x26', x27', x28', x29'),
          (x30', x31', x32', x33', x34'),
          (x35', x36', x37', x38', x39'),
          (x40')))
          <- shrink
               (((preShrink_Paths x00, preShrink_Args x01, x02, x03, x04),
                 (x05, x06, x07, x08, x09),
                 (x10, x11, map NonEmpty x12, x13, x14),
                 (x15, map NonEmpty x16,
                       map NonEmpty x17,
                       map NonEmpty x18,
                       x19)),
                ((x20, x21, x22, x23, x24),
                 (x25, x26, x27, x28, x29),
                 (x30, x31, x32, x33, x34),
                 (x35, x36, fmap NonEmpty x37, x38, fmap NonEmpty x39),
                 (x40)))
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


instance Arbitrary SourceRepo where
    arbitrary = (SourceRepo RepoThis
                           <$> arbitrary
                           <*> (fmap getShortToken <$> arbitrary)
                           <*> (fmap getShortToken <$> arbitrary)
                           <*> (fmap getShortToken <$> arbitrary)
                           <*> (fmap getShortToken <$> arbitrary)
                           <*> (fmap getShortToken <$> arbitrary))
                `suchThat` (/= emptySourceRepo)

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
      , repo /= emptySourceRepo
      ]

emptySourceRepo :: SourceRepo
emptySourceRepo = SourceRepo RepoThis Nothing Nothing Nothing
                                      Nothing Nothing Nothing


instance Arbitrary RepoType where
    arbitrary = elements knownRepoTypes

instance Arbitrary ReportLevel where
    arbitrary = elements [NoReports .. DetailedReports]

instance Arbitrary CompilerFlavor where
    arbitrary = elements knownCompilerFlavors
      where
        --TODO: [code cleanup] export knownCompilerFlavors from D.Compiler
        -- it's already defined there, just need it exported.
        knownCompilerFlavors =
          [GHC, GHCJS, NHC, YHC, Hugs, HBC, Helium, JHC, LHC, UHC]

instance Arbitrary a => Arbitrary (InstallDirs a) where
    arbitrary =
      InstallDirs
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary --  4
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary --  8
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 12
        <*> arbitrary <*> arbitrary                             -- 14

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

instance Arbitrary UserConstraint where
    arbitrary =
      oneof
        [ UserConstraintVersion   <$> arbitrary <*> arbitrary
        , UserConstraintInstalled <$> arbitrary
        , UserConstraintSource    <$> arbitrary
        , UserConstraintFlags     <$> arbitrary <*> shortListOf1 3 arbitrary
        , UserConstraintStanzas   <$> arbitrary <*> ((\x->[x]) <$> arbitrary)
        ]

instance Arbitrary OptionalStanza where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary FlagName where
    arbitrary = FlagName <$> flagident
      where
        flagident   = lowercase <$> shortListOf1 5 (elements flagChars)
                      `suchThat` (("-" /=) . take 1)
        flagChars   = "-_" ++ ['a'..'z']

instance Arbitrary PreSolver where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary AllowNewer where
    arbitrary = oneof [ pure AllowNewerNone
                      , AllowNewerSome <$> shortListOf1 3 arbitrary
                      , pure AllowNewerAll
                      ]

instance Arbitrary AllowNewerDep where
    arbitrary = oneof [ AllowNewerDep       <$> arbitrary
                      , AllowNewerDepScoped <$> arbitrary <*> arbitrary
                      ]

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

