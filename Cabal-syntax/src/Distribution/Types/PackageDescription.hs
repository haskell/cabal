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
-- This defines the data structure for the @.cabal@ file format. There are
-- several parts to this structure. It has top level info and then 'Library',
-- 'Executable', 'TestSuite', and 'Benchmark' sections each of which have
-- associated 'BuildInfo' data that's used to build the library, exe, test, or
-- benchmark.  To further complicate things there is both a 'PackageDescription'
-- and a 'GenericPackageDescription'. This distinction relates to cabal
-- configurations. When we initially read a @.cabal@ file we get a
-- 'GenericPackageDescription' which has all the conditional sections.
-- Before actually building a package we have to decide
-- on each conditional. Once we've done that we get a 'PackageDescription'.
-- It was done this way initially to avoid breaking too much stuff when the
-- feature was introduced. It could probably do with being rationalised at some
-- point to make it simpler.
module Distribution.Types.PackageDescription
  ( PackageDescription (..)
  , license
  , license'
  , buildType
  , emptyPackageDescription
  , hasPublicLib
  , hasLibs
  , allLibraries
  , withLib
  , hasExes
  , withExe
  , hasTests
  , withTest
  , hasBenchmarks
  , withBenchmark
  , hasForeignLibs
  , withForeignLib
  , allBuildInfo
  , enabledBuildInfos
  , allBuildDepends
  , enabledBuildDepends
  , updatePackageDescription
  , pkgComponents
  , pkgBuildableComponents
  , enabledComponents
  , lookupComponent
  , getComponent
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad ((<=<))

-- lens

import Distribution.Types.Benchmark
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.Executable
import Distribution.Types.ForeignLib
import Distribution.Types.Library
import Distribution.Types.TestSuite

import Distribution.Types.BuildInfo
import Distribution.Types.BuildType
import Distribution.Types.Component
import Distribution.Types.ComponentName
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.Dependency
import Distribution.Types.HookedBuildInfo
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.SetupBuildInfo
import Distribution.Types.SourceRepo
import Distribution.Types.UnqualComponentName

import Distribution.CabalSpecVersion
import Distribution.Compiler
import Distribution.License
import Distribution.Package
import Distribution.Utils.Path
import Distribution.Utils.ShortText
import Distribution.Version

import qualified Distribution.SPDX as SPDX

-- -----------------------------------------------------------------------------
-- The PackageDescription type

-- | This data type is the internal representation of the file @pkg.cabal@.
-- It contains two kinds of information about the package: information
-- which is needed for all packages, such as the package name and version, and
-- information which is needed for the simple build system only, such as
-- the compiler options and library name.
data PackageDescription = PackageDescription
  { -- the following are required by all packages:

    specVersion :: CabalSpecVersion
  -- ^ The version of the Cabal spec that this package description uses.
  , package :: PackageIdentifier
  , licenseRaw :: Either SPDX.License License
  , licenseFiles :: [SymbolicPath PackageDir LicenseFile]
  , copyright :: !ShortText
  , maintainer :: !ShortText
  , author :: !ShortText
  , stability :: !ShortText
  , testedWith :: [(CompilerFlavor, VersionRange)]
  , homepage :: !ShortText
  , pkgUrl :: !ShortText
  , bugReports :: !ShortText
  , sourceRepos :: [SourceRepo]
  , synopsis :: !ShortText
  -- ^ A one-line summary of this package
  , description :: !ShortText
  -- ^ A more verbose description of this package
  , category :: !ShortText
  , customFieldsPD :: [(String, String)]
  -- ^ Custom fields starting
  --  with x-, stored in a
  --  simple assoc-list.
  , buildTypeRaw :: Maybe BuildType
  -- ^ The original @build-type@ value as parsed from the
  -- @.cabal@ file without defaulting. See also 'buildType'.
  --
  -- @since 2.2
  , setupBuildInfo :: Maybe SetupBuildInfo
  , -- components
    library :: Maybe Library
  , subLibraries :: [Library]
  , executables :: [Executable]
  , foreignLibs :: [ForeignLib]
  , testSuites :: [TestSuite]
  , benchmarks :: [Benchmark]
  , -- files
    dataFiles :: [FilePath]
  , dataDir :: FilePath
  , extraSrcFiles :: [FilePath]
  , extraTmpFiles :: [FilePath]
  , extraDocFiles :: [FilePath]
  }
  deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)

instance Binary PackageDescription
instance Structured PackageDescription

instance NFData PackageDescription where rnf = genericRnf

instance Package PackageDescription where
  packageId = package

-- | The SPDX 'LicenseExpression' of the package.
--
-- @since 2.2.0.0
license :: PackageDescription -> SPDX.License
license = license' . licenseRaw

-- | See 'license'.
--
-- @since 2.2.0.0
license' :: Either SPDX.License License -> SPDX.License
license' = either id licenseToSPDX

-- | The effective @build-type@ after applying defaulting rules.
--
-- The original @build-type@ value parsed is stored in the
-- 'buildTypeRaw' field.  However, the @build-type@ field is optional
-- and can therefore be empty in which case we need to compute the
-- /effective/ @build-type@. This function implements the following
-- defaulting rules:
--
--  * For @cabal-version:2.0@ and below, default to the @Custom@
--    build-type unconditionally.
--
--  * Otherwise, if a @custom-setup@ stanza is defined, default to
--    the @Custom@ build-type; else default to @Simple@ build-type.
--
-- @since 2.2
buildType :: PackageDescription -> BuildType
buildType pkg
  | specVersion pkg >= CabalSpecV2_2 =
      fromMaybe newDefault (buildTypeRaw pkg)
  | otherwise -- cabal-version < 2.1
    =
      fromMaybe Custom (buildTypeRaw pkg)
  where
    newDefault
      | isNothing (setupBuildInfo pkg) = Simple
      | otherwise = Custom

emptyPackageDescription :: PackageDescription
emptyPackageDescription =
  PackageDescription
    { package =
        PackageIdentifier
          (mkPackageName "")
          nullVersion
    , licenseRaw = Right UnspecifiedLicense -- TODO:
    , licenseFiles = []
    , specVersion = CabalSpecV1_0
    , buildTypeRaw = Nothing
    , copyright = mempty
    , maintainer = mempty
    , author = mempty
    , stability = mempty
    , testedWith = []
    , homepage = mempty
    , pkgUrl = mempty
    , bugReports = mempty
    , sourceRepos = []
    , synopsis = mempty
    , description = mempty
    , category = mempty
    , customFieldsPD = []
    , setupBuildInfo = Nothing
    , library = Nothing
    , subLibraries = []
    , foreignLibs = []
    , executables = []
    , testSuites = []
    , benchmarks = []
    , dataFiles = []
    , dataDir = "."
    , extraSrcFiles = []
    , extraTmpFiles = []
    , extraDocFiles = []
    }

-- ---------------------------------------------------------------------------
-- The Library type

-- | Does this package have a buildable PUBLIC library?
hasPublicLib :: PackageDescription -> Bool
hasPublicLib p =
  case library p of
    Just lib -> buildable (libBuildInfo lib)
    Nothing -> False

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

-- | does this package have any executables?
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
  sequence_ [f test | test <- testSuites pkg_descr, buildable (testBuildInfo test)]

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
  sequence_
    [ f flib
    | flib <- foreignLibs pkg_descr
    , buildable (foreignLibBuildInfo flib)
    ]

-- ---------------------------------------------------------------------------
-- The BuildInfo type

-- | All 'BuildInfo' in the 'PackageDescription':
-- libraries, executables, test-suites and benchmarks.
--
-- Useful for implementing package checks.
allBuildInfo :: PackageDescription -> [BuildInfo]
allBuildInfo pkg_descr =
  [ bi | lib <- allLibraries pkg_descr, let bi = libBuildInfo lib
  ]
    ++ [ bi | flib <- foreignLibs pkg_descr, let bi = foreignLibBuildInfo flib
       ]
    ++ [ bi | exe <- executables pkg_descr, let bi = buildInfo exe
       ]
    ++ [ bi | tst <- testSuites pkg_descr, let bi = testBuildInfo tst
       ]
    ++ [ bi | tst <- benchmarks pkg_descr, let bi = benchmarkBuildInfo tst
       ]

-- | Return all of the 'BuildInfo's of enabled components, i.e., all of
-- the ones that would be built if you run @./Setup build@.
enabledBuildInfos :: PackageDescription -> ComponentRequestedSpec -> [BuildInfo]
enabledBuildInfos pkg enabled =
  [ componentBuildInfo comp
  | comp <- enabledComponents pkg enabled
  ]

-- ------------------------------------------------------------

-- * Utils

-- ------------------------------------------------------------

-- | Get the combined build-depends entries of all components.
allBuildDepends :: PackageDescription -> [Dependency]
allBuildDepends = targetBuildDepends <=< allBuildInfo

-- | Get the combined build-depends entries of all enabled components, per the
-- given request spec.
enabledBuildDepends :: PackageDescription -> ComponentRequestedSpec -> [Dependency]
enabledBuildDepends spec pd = targetBuildDepends =<< enabledBuildInfos spec pd

updatePackageDescription :: HookedBuildInfo -> PackageDescription -> PackageDescription
updatePackageDescription (mb_lib_bi, exe_bi) p =
  p
    { executables = updateExecutables exe_bi (executables p)
    , library = updateLibrary mb_lib_bi (library p)
    }
  where
    updateLibrary :: Maybe BuildInfo -> Maybe Library -> Maybe Library
    updateLibrary (Just bi) (Just lib) = Just (lib{libBuildInfo = bi `mappend` libBuildInfo lib})
    updateLibrary Nothing mb_lib = mb_lib
    updateLibrary (Just _) Nothing = Nothing

    updateExecutables
      :: [(UnqualComponentName, BuildInfo)]
      -- \^[(exeName, new buildinfo)]
      -> [Executable]
      -- \^list of executables to update
      -> [Executable]
    -- \^list with exeNames updated
    updateExecutables exe_bi' executables' = foldr updateExecutable executables' exe_bi'

    updateExecutable
      :: (UnqualComponentName, BuildInfo)
      -- \^(exeName, new buildinfo)
      -> [Executable]
      -- \^list of executables to update
      -> [Executable]
    -- \^list with exeName updated
    updateExecutable _ [] = []
    updateExecutable exe_bi'@(name, bi) (exe : exes)
      | exeName exe == name = exe{buildInfo = bi `mappend` buildInfo exe} : exes
      | otherwise = exe : updateExecutable exe_bi' exes

-- -----------------------------------------------------------------------------
-- Source-representation of buildable components

-- | All the components in the package.
pkgComponents :: PackageDescription -> [Component]
pkgComponents pkg =
  [CLib lib | lib <- allLibraries pkg]
    ++ [CFLib flib | flib <- foreignLibs pkg]
    ++ [CExe exe | exe <- executables pkg]
    ++ [CTest tst | tst <- testSuites pkg]
    ++ [CBench bm | bm <- benchmarks pkg]

-- | A list of all components in the package that are buildable,
-- i.e., were not marked with @buildable: False@.  This does NOT
-- indicate if we are actually going to build the component,
-- see 'enabledComponents' instead.
--
-- @since 2.0.0.2
pkgBuildableComponents :: PackageDescription -> [Component]
pkgBuildableComponents = filter componentBuildable . pkgComponents

-- | A list of all components in the package that are enabled.
--
-- @since 2.0.0.2
enabledComponents :: PackageDescription -> ComponentRequestedSpec -> [Component]
enabledComponents pkg enabled = filter (componentEnabled enabled) $ pkgBuildableComponents pkg

lookupComponent :: PackageDescription -> ComponentName -> Maybe Component
lookupComponent pkg (CLibName name) =
  fmap CLib $ find ((name ==) . libName) (allLibraries pkg)
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
    Nothing -> missingComponent
  where
    missingComponent =
      error $
        "internal error: the package description contains no "
          ++ "component corresponding to "
          ++ show cname

-- -----------------------------------------------------------------------------
-- Traversal Instances

instance L.HasBuildInfos PackageDescription where
  traverseBuildInfos
    f
    ( PackageDescription
        a1
        a2
        a3
        a4
        a5
        a6
        a7
        a8
        a9
        a10
        a11
        a12
        a13
        a14
        a15
        a16
        a17
        a18
        a19
        x1
        x2
        x3
        x4
        x5
        x6
        a20
        a21
        a22
        a23
        a24
      ) =
      PackageDescription a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19
        <$> (traverse . L.buildInfo) f x1 -- library
        <*> (traverse . L.buildInfo) f x2 -- sub libraries
        <*> (traverse . L.buildInfo) f x3 -- executables
        <*> (traverse . L.buildInfo) f x4 -- foreign libs
        <*> (traverse . L.buildInfo) f x5 -- test suites
        <*> (traverse . L.buildInfo) f x6 -- benchmarks
        <*> pure a20 -- data files
        <*> pure a21 -- data dir
        <*> pure a22 -- extra src files
        <*> pure a23 -- extra temp files
        <*> pure a24 -- extra doc files
