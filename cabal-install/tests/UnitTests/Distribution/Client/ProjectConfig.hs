{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Client.ProjectConfig (tests) where

import Data.Monoid
import Control.Applicative
import Data.Map (Map)

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Compiler
import Distribution.ParseUtils
import Distribution.Simple.Compiler
import Distribution.Simple.Setup
import Distribution.Simple.InstallDirs

import Distribution.Client.Types
import Distribution.Client.Dependency.Types
import Distribution.Client.BuildReports.Types
import Distribution.Client.Targets
import Network.URI

import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Legacy

import UnitTests.Distribution.Client.ArbitraryInstances

import Test.Tasty
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testGroup "ProjectConfig <-> LegacyProjectConfig round trip"
    [ testProperty "packages"  prop_roundtrip_legacytypes_packages
    , testProperty "buildonly" prop_roundtrip_legacytypes_buildonly
    , testProperty "shared"    prop_roundtrip_legacytypes_shared
    , testProperty "local"     prop_roundtrip_legacytypes_local
    , testProperty "specific"  prop_roundtrip_legacytypes_specific
    , testProperty "all"       prop_roundtrip_legacytypes_all
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
        projectPackagesRepo          = mempty,
        projectPackagesNamed         = mempty,
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
      mempty { projectConfigSpecificPackage = config }


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
      ParseOk  [] x -> x == config
      _             -> False


prop_roundtrip_printparse_all :: ProjectConfig -> Bool
prop_roundtrip_printparse_all =
    roundtrip_printparse

prop_roundtrip_printparse_packages :: ProjectConfig -> Bool
prop_roundtrip_printparse_packages config =
    roundtrip_printparse
      config {
        projectPackagesRepo          = mempty,
        projectPackagesNamed         = mempty,
        projectConfigBuildOnly       = mempty,
        projectConfigShared          = mempty,
        projectConfigLocalPackages   = mempty,
        projectConfigSpecificPackage = mempty
      }

prop_roundtrip_printparse_buildonly :: ProjectConfigBuildOnly -> Bool
prop_roundtrip_printparse_buildonly config =
    roundtrip_printparse
      mempty { projectConfigBuildOnly = config }

prop_roundtrip_printparse_shared :: ProjectConfigShared -> Bool
prop_roundtrip_printparse_shared config =
    roundtrip_printparse
      mempty { projectConfigShared = config }

prop_roundtrip_printparse_local :: PackageConfig -> Bool
prop_roundtrip_printparse_local config =
    roundtrip_printparse
      mempty { projectConfigLocalPackages = config }

prop_roundtrip_printparse_specific :: Map PackageName PackageConfig -> Bool
prop_roundtrip_printparse_specific config =
    roundtrip_printparse
      mempty { projectConfigSpecificPackage = config }


------------------------
-- Arbitrary instances
--

instance Arbitrary ProjectConfig where
    arbitrary =
      ProjectConfig
        <$> arbitrary --PackageLocationString
        <*> arbitrary --PackageLocationString
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary

    shrink (ProjectConfig x0 x1 x2 x3 x4 x5 x6 x7) =
      [ ProjectConfig x0' x1' x2' x3' x4' x5' x6' x7'
      | ((x0', x1', x2', x3'), (x4', x5', x6', x7'))
          <- shrink ((x0, x1, x2, x3), (x4, x5, x6, x7))
      ]

instance Arbitrary ProjectConfigBuildOnly where
    arbitrary =
      ProjectConfigBuildOnly
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary --  4
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary --  8
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 12
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 16
        <*> arbitrary <*> arbitrary

    shrink (ProjectConfigBuildOnly
              x00 x01 x02 x03 x04 x05 x06 x07
              x08 x09 x10 x11 x12 x13 x14 x15
              x16 x17) =
      [ ProjectConfigBuildOnly
          x00' x01' x02' x03' x04'
          x05' x06' x07' x08' x09'
          x10' x11' x12' x13' x14'
          x15' x16' x17'
      | ((x00', x01', x02', x03', x04'),
         (x05', x06', x07', x08', x09'),
         (x10', x11', x12', x13', x14'),
         (x15', x16', x17'))
          <- shrink
               ((x00, x01, x02, x03, x04),
                (x05, x06, x07, x08, x09),
                (x10, x11, x12, x13, x14),
                (x15, x16, x17))
      ]

instance Arbitrary ProjectConfigShared where
    arbitrary =
      ProjectConfigShared
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary --  4
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary --  8
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 12
        <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitraryConstraints                                -- 16
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 20
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 24
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 28
        <*> arbitrary <*> arbitrary                             -- 30
      where
        arbitraryConstraints :: Gen [(UserConstraint, ConstraintSource)]
        arbitraryConstraints =
            map (\uc -> (uc, projectConfigConstraintSource)) <$> arbitrary

    shrink (ProjectConfigShared
              x00 x01 x02 x03 x04
              x05 x06 x07 x08 x09
              x10 x11 x12 x13 x14
              x15 x16 x17 x18 x19
              x20 x21 x22 x23 x24
              x25 x26 x27 x28 x29) =
      [ ProjectConfigShared
          x00' x01' x02' x03' x04'
          x05' x06' x07' x08' x09'
          x10' x11' x12' x13' x14'
          (map (\uc -> (uc, projectConfigConstraintSource)) x15')
               x16' x17' x18' x19'
          x20' x21' x22' x23' x24'
          x25' x26' x27' x28' x29'
      | (((x00', x01', x02', x03', x04'),
          (x05', x06', x07', x08', x09'),
          (x10', x11', x12', x13', x14'),
          (x15', x16', x17', x18', x19')),
         ((x20', x21', x22', x23', x24'),
          (x25', x26', x27', x28', x29')))
          <- shrink
               (((x00, x01, x02, x03, x04),
                 (x05, x06, x07, x08, x09),
                 (x10, x11, x12, x13, x14),
                 (map fst x15,
                       x16, x17, x18, x19)),
                ((x20, x21, x22, x23, x24),
                 (x25, x26, x27, x28, x29)))
      ]

projectConfigConstraintSource :: ConstraintSource
projectConfigConstraintSource = 
    ConstraintSourceProjectConfig "???"

instance Arbitrary PackageConfig where
    arbitrary =
      PackageConfig
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary --  4
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary --  8
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 12
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 16
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 20
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 24
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 28
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 32
        <*> arbitrary <*> arbitrary                             -- 34

    shrink (PackageConfig
              x00 x01 x02 x03 x04
              x05 x06 x07 x08 x09
              x10 x11 x12 x13 x14
              x15 x16 x17 x18 x19
              x20 x21 x22 x23 x24
              x25 x26 x27 x28 x29
              x30 x31 x32 x33) =
      [ PackageConfig
          x00' x01' x02' x03' x04'
          x05' x06' x07' x08' x09'
          x10' x11' x12' x13' x14'
          x15' x16' x17' x18' x19'
          x20' x21' x22' x23' x24'
          x25' x26' x27' x28' x29'
          x30' x31' x32' x33'
      | (((x00', x01', x02', x03', x04'),
          (x05', x06', x07', x08', x09'),
          (x10', x11', x12', x13', x14'),
          (x15', x16', x17', x18', x19')),
         ((x20', x21', x22', x23', x24'),
          (x25', x26', x27', x28', x29'),
          (x30', x31', x32', x33')))
          <- shrink
               (((x00, x01, x02, x03, x04),
                 (x05, x06, x07, x08, x09),
                 (x10, x11, x12, x13, x14),
                 (x15, x16, x17, x18, x19)),
                ((x20, x21, x22, x23, x24),
                 (x25, x26, x27, x28, x29),
                 (x30, x31, x32, x33)))
      ]


instance Arbitrary SourceRepo where
    arbitrary = SourceRepo RepoThis
                           <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

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
                      , SpecificPackageDB <$> arbitraryShortToken
                      ]

instance Arbitrary RemoteRepo where
    arbitrary =
      RemoteRepo
        <$> arbitraryShortToken
        <*> arbitrary  -- URI
        <*> arbitrary
        <*> listOf arbitraryShortToken -- root keys
        <*> arbitrary
        <*> pure False

instance Arbitrary UserConstraint where
    arbitrary = oneof [ UserConstraintVersion   <$> arbitrary <*> arbitrary
                      , UserConstraintInstalled <$> arbitrary
                      , UserConstraintSource    <$> arbitrary
                      , UserConstraintFlags     <$> arbitrary <*> arbitrary
                      , UserConstraintStanzas   <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary OptionalStanza where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary FlagName where
    arbitrary = FlagName <$> arbitraryShortToken

instance Arbitrary PreSolver where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary AllowNewer where
    arbitrary = oneof [ pure AllowNewerNone
                      , AllowNewerSome <$> arbitrary
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
    shortListOf1 (elements (filter isUnreserved ['\0'..'\255']))

arbitraryURIPort :: Gen String
arbitraryURIPort =
    oneof [ pure "", (':':) <$> shortListOf1 (choose ('0','9')) ]

