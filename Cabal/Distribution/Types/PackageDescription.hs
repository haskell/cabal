{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Types.PackageDescription
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is one of the modules that defines the data structure for the @.cabal@
-- file format. There is both a 'PackageDescription' and a
-- 'GenericPackageDescription'; see the documentation for each to understand the
-- division of labor. It was done this way initially to avoid breaking too much
-- stuff when the feature was introduced. It could probably do with being
-- rationalised at some point to make it simpler.

module Distribution.Types.PackageDescription (
    PackageDescription(..),
    descCabalVersion,
    emptyPackageDescription,
    hasPublicLib,
    hasLibs,
    allLibraries,
    withLib,
    hasExes,
    withExe,
    hasTests,
    withTest,
    hasBenchmarks,
    withBenchmark,
    hasForeignLibs,
    withForeignLib,
    enabledBuildInfos,
    enabledBuildDepends,
    updatePackageDescription,
    pkgComponents,
    pkgBuildableComponents,
    enabledComponents,
    lookupComponent,
    getComponent,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

-- lens
import qualified Distribution.Types.Benchmark.Lens  as L
import qualified Distribution.Types.BuildInfo.Lens  as L
import qualified Distribution.Types.CommonPackageDescription.Lens as L
import qualified Distribution.Types.Executable.Lens as L
import qualified Distribution.Types.ForeignLib.Lens as L
import qualified Distribution.Types.Library.Lens    as L
import qualified Distribution.Types.TestSuite.Lens  as L

import Distribution.Types.Library
import Distribution.Types.TestSuite
import Distribution.Types.Executable
import Distribution.Types.Benchmark
import Distribution.Types.ForeignLib

import Distribution.Types.CommonPackageDescription
import Distribution.Types.Component
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.Dependency
import Distribution.Types.ComponentName
import Distribution.Types.UnqualComponentName
import Distribution.Types.BuildInfo
import Distribution.Types.BuildType
import Distribution.Types.HookedBuildInfo

import Distribution.License
import Distribution.Package
import Distribution.Version

import qualified Distribution.SPDX as SPDX

-- -----------------------------------------------------------------------------
-- The PackageDescription type

-- | This data type is the abstract representation of the file @pkg.cabal@.
--
-- There are several parts to this structure. It has top level info and then
-- 'Library', 'Executable', 'TestSuite', and 'Benchmark' sections each of which
-- have associated 'BuildInfo' data that's used to build the library, exe, test,
-- or benchmark.
--
-- When we initially read a @.cabal@ file we get a 'GenericPackageDescription'
-- which has all the conditional sections.  Before actually building a package
-- we have to decide on each conditional. Once we've done that we get a
-- 'PackageDescription'. For this reason, this is the more resolved
-- representation, farther through the pipeline.
data PackageDescription
    =  PackageDescription {
         -- | Fields shared with 'GenericPackageDescription'
         --
         -- The "generic" in 'genericCommonPD' disambiguates it from the field of
         -- 'PackageDescription'.
         --
         -- @since 2.6
        commonPD       :: CommonPackageDescription,

        -- the following are required by all packages:

        -- | The version of the Cabal spec that this package should be
        -- interpreted against.
        specVersion    :: Version,
        license        :: SPDX.License,

        -- | Mandatory build type
        -- @since 2.2
        buildType      :: BuildType,
        -- components
        library        :: Maybe Library,
        subLibraries   :: [Library],
        executables    :: [Executable],
        foreignLibs    :: [ForeignLib],
        testSuites     :: [TestSuite],
        benchmarks     :: [Benchmark]
    }
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary PackageDescription

instance NFData PackageDescription where rnf = genericRnf

instance L.HasCommonPackageDescription PackageDescription where
  commonPackageDescription f l = (\x -> l { commonPD = x }) <$> f (commonPD l)

instance Package PackageDescription where
  packageId = package . commonPD

-- | The range of versions of the Cabal tools that this package is intended to
-- work with.
--
-- This function is deprecated and should not be used for new purposes, only to
-- support old packages that rely on the old interpretation.
--
descCabalVersion :: PackageDescription -> VersionRange
descCabalVersion = orLaterVersion . specVersion
{-# DEPRECATED descCabalVersion "Use specVersion instead. This symbol will be removed in Cabal-3.0 (est. Oct 2018)." #-}

emptyPackageDescription :: PackageDescription
emptyPackageDescription
    =  PackageDescription {
                      commonPD     = emptyCommonPackageDescription,
                      license      = licenseToSPDX $ UnspecifiedLicense,
                      specVersion  = mkVersion [0],
                      buildType    = Custom, -- the legacy default
                      library      = Nothing,
                      subLibraries = [],
                      foreignLibs  = [],
                      executables  = [],
                      testSuites   = [],
                      benchmarks   = []
                     }

-- ---------------------------------------------------------------------------
-- The Library type

-- | Does this package have a buildable PUBLIC library?
hasPublicLib :: PackageDescription -> Bool
hasPublicLib p =
    case library p of
        Just lib -> buildable (libBuildInfo lib)
        Nothing  -> False

-- | Does this package have any libraries?
hasLibs :: PackageDescription -> Bool
hasLibs p = any (buildable . libBuildInfo) (allLibraries p)

allLibraries :: PackageDescription -> [Library]
allLibraries p = maybeToList (library p) ++ subLibraries p

-- | If the package description has a buildable library section,
-- call the given function with the library build info as argument.
-- You probably want 'withLibLBI' if you have a 'LocalBuildInfo',
-- see the note in
-- "Distribution.Types.ComponentRequestedSpec#buildable_vs_enabled_components"
-- for more information.
withLib :: PackageDescription -> (Library -> IO ()) -> IO ()
withLib pkg_descr f =
   sequence_ [f lib | lib <- allLibraries pkg_descr, buildable (libBuildInfo lib)]

-- ---------------------------------------------------------------------------
-- The Executable type

-- |does this package have any executables?
hasExes :: PackageDescription -> Bool
hasExes p = any (buildable . buildInfo) (executables p)

-- | Perform the action on each buildable 'Executable' in the package
-- description.  You probably want 'withExeLBI' if you have a
-- 'LocalBuildInfo', see the note in
-- "Distribution.Types.ComponentRequestedSpec#buildable_vs_enabled_components"
-- for more information.
withExe :: PackageDescription -> (Executable -> IO ()) -> IO ()
withExe pkg_descr f =
  sequence_ [f exe | exe <- executables pkg_descr, buildable (buildInfo exe)]

-- ---------------------------------------------------------------------------
-- The TestSuite type

-- | Does this package have any test suites?
hasTests :: PackageDescription -> Bool
hasTests = any (buildable . testBuildInfo) . testSuites

-- | Perform an action on each buildable 'TestSuite' in a package.
-- You probably want 'withTestLBI' if you have a 'LocalBuildInfo', see the note in
-- "Distribution.Types.ComponentRequestedSpec#buildable_vs_enabled_components"
-- for more information.

withTest :: PackageDescription -> (TestSuite -> IO ()) -> IO ()
withTest pkg_descr f =
    sequence_ [ f test | test <- testSuites pkg_descr, buildable (testBuildInfo test) ]

-- ---------------------------------------------------------------------------
-- The Benchmark type

-- | Does this package have any benchmarks?
hasBenchmarks :: PackageDescription -> Bool
hasBenchmarks = any (buildable . benchmarkBuildInfo) . benchmarks

-- | Perform an action on each buildable 'Benchmark' in a package.
-- You probably want 'withBenchLBI' if you have a 'LocalBuildInfo', see the note in
-- "Distribution.Types.ComponentRequestedSpec#buildable_vs_enabled_components"
-- for more information.

withBenchmark :: PackageDescription -> (Benchmark -> IO ()) -> IO ()
withBenchmark pkg_descr f =
    sequence_ [f bench | bench <- benchmarks pkg_descr, buildable (benchmarkBuildInfo bench)]

-- ---------------------------------------------------------------------------
-- The ForeignLib type

-- | Does this package have any foreign libraries?
hasForeignLibs :: PackageDescription -> Bool
hasForeignLibs p = any (buildable . foreignLibBuildInfo) (foreignLibs p)

-- | Perform the action on each buildable 'ForeignLib' in the package
-- description.
withForeignLib :: PackageDescription -> (ForeignLib -> IO ()) -> IO ()
withForeignLib pkg_descr f =
  sequence_ [ f flib
            | flib <- foreignLibs pkg_descr
            , buildable (foreignLibBuildInfo flib)
            ]

-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

-- | Return all of the 'BuildInfo's of enabled components, i.e., all of
-- the ones that would be built if you run @./Setup build@.
enabledBuildInfos :: PackageDescription -> ComponentRequestedSpec -> [BuildInfo]
enabledBuildInfos pkg enabled =
    [ componentBuildInfo comp
    | comp <- enabledComponents pkg enabled ]

-- | Get the combined build-depends entries of all enabled components, per the
-- given request spec.
enabledBuildDepends :: PackageDescription -> ComponentRequestedSpec -> [Dependency]
enabledBuildDepends spec pd = targetBuildDepends =<< enabledBuildInfos spec pd


updatePackageDescription :: HookedBuildInfo -> PackageDescription -> PackageDescription
updatePackageDescription (mb_lib_bi, exe_bi) p
    = p{ executables = updateExecutables exe_bi    (executables p)
       , library     = updateLibrary     mb_lib_bi (library     p) }
    where
      updateLibrary :: Maybe BuildInfo -> Maybe Library -> Maybe Library
      updateLibrary (Just bi) (Just lib) = Just (lib{libBuildInfo = bi `mappend` libBuildInfo lib})
      updateLibrary Nothing   mb_lib     = mb_lib
      updateLibrary (Just _)  Nothing    = Nothing

      updateExecutables :: [(UnqualComponentName, BuildInfo)] -- ^[(exeName, new buildinfo)]
        -> [Executable]                                       -- ^list of executables to update
        -> [Executable]                                       -- ^list with exeNames updated
      updateExecutables exe_bi' executables' = foldr updateExecutable executables' exe_bi'

      updateExecutable :: (UnqualComponentName, BuildInfo) -- ^(exeName, new buildinfo)
                       -> [Executable]                     -- ^list of executables to update
                       -> [Executable]                     -- ^list with exeName updated
      updateExecutable _                 []         = []
      updateExecutable exe_bi'@(name,bi) (exe:exes)
        | exeName exe == name = exe{buildInfo = bi `mappend` buildInfo exe} : exes
        | otherwise           = exe : updateExecutable exe_bi' exes

-- -----------------------------------------------------------------------------
-- Source-representation of buildable components

-- | All the components in the package.
--
pkgComponents :: PackageDescription -> [Component]
pkgComponents pkg =
    [ CLib  lib | lib <- allLibraries pkg ]
 ++ [ CFLib flib | flib <- foreignLibs pkg ]
 ++ [ CExe  exe | exe <- executables pkg ]
 ++ [ CTest tst | tst <- testSuites  pkg ]
 ++ [ CBench bm | bm  <- benchmarks  pkg ]

-- | A list of all components in the package that are buildable,
-- i.e., were not marked with @buildable: False@.  This does NOT
-- indicate if we are actually going to build the component,
-- see 'enabledComponents' instead.
--
-- @since 2.0.0.2
--
pkgBuildableComponents :: PackageDescription -> [Component]
pkgBuildableComponents = filter componentBuildable . pkgComponents

-- | A list of all components in the package that are enabled.
--
-- @since 2.0.0.2
--
enabledComponents :: PackageDescription -> ComponentRequestedSpec -> [Component]
enabledComponents pkg enabled = filter (componentEnabled enabled) $ pkgBuildableComponents pkg

lookupComponent :: PackageDescription -> ComponentName -> Maybe Component
lookupComponent pkg CLibName = fmap CLib (library pkg)
lookupComponent pkg (CSubLibName name) =
    fmap CLib $ find ((Just name ==) . libName) (subLibraries pkg)
lookupComponent pkg (CFLibName name) =
    fmap CFLib $ find ((name ==) . foreignLibName) (foreignLibs pkg)
lookupComponent pkg (CExeName name) =
    fmap CExe $ find ((name ==) . exeName) (executables pkg)
lookupComponent pkg (CTestName name) =
    fmap CTest $ find ((name ==) . testName) (testSuites pkg)
lookupComponent pkg (CBenchName name) =
    fmap CBench $ find ((name ==) . benchmarkName) (benchmarks pkg)

getComponent :: PackageDescription -> ComponentName -> Component
getComponent pkg cname =
    case lookupComponent pkg cname of
      Just cpnt -> cpnt
      Nothing   -> missingComponent
  where
    missingComponent =
      error $ "internal error: the package description contains no "
           ++ "component corresponding to " ++ show cname

-- -----------------------------------------------------------------------------
-- Traversal Instances

instance L.HasBuildInfos PackageDescription where
  traverseBuildInfos f (PackageDescription p a1 a2 a3 x1 x2 x3 x4 x5 x6) =
    PackageDescription p a1 a2 a3
        <$> (traverse . L.buildInfo) f x1 -- library
        <*> (traverse . L.buildInfo) f x2 -- sub libraries
        <*> (traverse . L.buildInfo) f x3 -- executables
        <*> (traverse . L.buildInfo) f x4 -- foreign libs
        <*> (traverse . L.buildInfo) f x5 -- test suites
        <*> (traverse . L.buildInfo) f x6 -- benchmarks

instance L.HasLibraries PackageDescription where
  traverseLibraries f (PackageDescription p a1 a2 a3 x1 x2 x3 x4 x5 x6) =
    PackageDescription p a1 a2 a3
        <$> traverse f x1  -- library
        <*> traverse f x2  -- sub libraries
        <*> pure x3        -- executables
        <*> pure x4        -- foreign libs
        <*> pure x5        -- test suites
        <*> pure x6        -- benchmarks

instance L.HasExecutables PackageDescription where
  traverseExecutables f s = fmap (\x -> s { executables = x }) (traverse f (executables s))

instance L.HasForeignLibs PackageDescription where
  traverseForeignLibs f s = fmap (\x -> s { foreignLibs = x }) (traverse f (foreignLibs s))

instance L.HasTestSuites PackageDescription where
  traverseTestSuites f s = fmap (\x -> s { testSuites = x }) (traverse f (testSuites s))

instance L.HasBenchmarks PackageDescription where
  traverseBenchmarks f s = fmap (\x -> s { benchmarks = x }) (traverse f (benchmarks s))

instance L.IsPackageDescription PackageDescription where
  lensSpecVersion f s = fmap (\x -> s { specVersion = x }) (f (specVersion s))
  {-# INLINE lensSpecVersion #-}
  
  lensLicense f s = fmap (\x -> s { license = x }) (f (license s))
  {-# INLINE lensLicense #-}
  
  lensBuildType f s = fmap (\x -> s { buildType = x }) (f (buildType s))
  {-# INLINE lensBuildType #-}

  traversePublicLib = lens . traverse
    where lens f s = fmap (\x -> s { library = x }) (f (library s))
  {-# INLINE traversePublicLib #-}

  traverseSubLibs = lens . traverse
    where lens f s = fmap (\x -> s { subLibraries = x }) (f (subLibraries s))
  {-# INLINE traverseSubLibs #-}
