{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
#if __GLASGOW_HASKELL__ >= 914
{-# LANGUAGE ExplicitNamespaces #-}
#endif

module Distribution.Types.GenericPackageDescription
  ( GenericPackageDescription (..)
#if __GLASGOW_HASKELL__ >= 914
  , data GenericPackageDescription
#else
  , pattern GenericPackageDescription
#endif
  , emptyGenericPackageDescription
  , mergeImports

    -- * Accessors from 'PatternSynonyms'\'s record syntax
  , packageDescription
  , gpdScannedVersion
  , genPackageFlags
  , condLibrary
  , condSubLibraries
  , condForeignLibs
  , condExecutables
  , condTestSuites
  , condBenchmarks

    -- * Merging helpers
  , mergeCondLibrary
  , mergeCondSubLibraries
  , mergeCondForeignLibs
  , mergeCondExecutables
  , mergeTestSuiteStanza
  , mergeBenchmarkStanza
  ) where

import Distribution.Compat.Prelude
import Prelude ()

-- lens
import Distribution.Compat.Lens as L
import qualified Distribution.Types.BuildInfo.Lens as L

-- TODO(leana8959): fix it this orphan
import qualified Distribution.Types.Imports.Lens as L ()

import Distribution.Types.PackageDescription

import Distribution.CabalSpecVersion
import Distribution.Package
import Distribution.Types.Benchmark
import Distribution.Types.BenchmarkStanza
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.ConfVar
import Distribution.Types.Executable
import Distribution.Types.Flag
import Distribution.Types.ForeignLib
import Distribution.Types.Imports
import Distribution.Types.Library
import Distribution.Types.LibraryName
import Distribution.Types.LibraryVisibility
import Distribution.Types.TestSuite
import Distribution.Types.TestSuiteStanza
import Distribution.Types.UnqualComponentName
import Distribution.Version

import qualified Data.Map as Map

-- ---------------------------------------------------------------------------
-- The 'GenericPackageDescription' type

type DependencyTree a = CondTree ConfVar [Dependency] a

-- | The internal representation of 'GenericPackageDescription', containing the unmerged stanzas
-- We provide a pattern below for backward compatibility, as well for hiding the internals of wiring the imports
data GenericPackageDescription = GenericPackageDescription'
  { packageDescriptionInternal :: PackageDescription
  , gpdScannedVersionInternal :: Maybe Version
  -- ^ This is a version as specified in source.
  --   We populate this field in index reading for dummy GPDs,
  --   only when GPD reading failed, but scanning haven't.
  --
  --   Cabal-the-library never produces GPDs with Just as gpdScannedVersion.
  --
  --   Perfectly, PackageIndex should have sum type, so we don't need to
  --   have dummy GPDs.
  , genPackageFlagsInternal :: [PackageFlag]
  , gpdCommonStanzas :: Map ImportName (DependencyTree (WithImports BuildInfo))
  , condLibraryUnmerged :: Maybe (DependencyTree (WithImports Library))
  , condSubLibrariesUnmerged :: [(UnqualComponentName, DependencyTree (WithImports Library))]
  , condForeignLibsUnmerged :: [(UnqualComponentName, DependencyTree (WithImports ForeignLib))]
  , condExecutablesUnmerged :: [(UnqualComponentName, DependencyTree (WithImports Executable))]
  , condTestSuitesUnmerged :: [(UnqualComponentName, DependencyTree (WithImports TestSuiteStanza))]
  , condBenchmarksUnmerged :: [(UnqualComponentName, DependencyTree (WithImports BenchmarkStanza))]
  }
  deriving (Show, Eq, Data, Generic)

pattern GenericPackageDescription
  :: PackageDescription
  -> Maybe Version
  -> [PackageFlag]
  -> Maybe (DependencyTree Library)
  -> [(UnqualComponentName, DependencyTree Library)]
  -> [(UnqualComponentName, DependencyTree ForeignLib)]
  -> [(UnqualComponentName, DependencyTree Executable)]
  -> [(UnqualComponentName, DependencyTree TestSuite)]
  -> [(UnqualComponentName, DependencyTree Benchmark)]
  -> GenericPackageDescription
pattern GenericPackageDescription
  { packageDescription
  , gpdScannedVersion
  , genPackageFlags
  , condLibrary
  , condSubLibraries
  , condForeignLibs
  , condExecutables
  , condTestSuites
  , condBenchmarks
  } <-
  ( viewGenericPackageDescription ->
      ( packageDescription
        , gpdScannedVersion
        , genPackageFlags
        , condLibrary
        , condSubLibraries
        , condForeignLibs
        , condExecutables
        , condTestSuites
        , condBenchmarks
        )
    )
  where
    GenericPackageDescription
      pd
      scannedVersion
      packageFlags
      lib
      sublibs
      flibs
      exes
      tests
      bms =
        GenericPackageDescription'
          pd
          scannedVersion
          packageFlags
          mempty
          ((fmap . fmap) noImports lib)
          ((fmap . fmap . fmap) noImports sublibs)
          ((fmap . fmap . fmap) noImports flibs)
          ((fmap . fmap . fmap) noImports exes)
          ((fmap . fmap . fmap) (noImports . unvalidateTestSuite) tests)
          ((fmap . fmap . fmap) (noImports . unvalidateBenchmark) bms)

{-# COMPLETE GenericPackageDescription #-}

viewGenericPackageDescription
  :: GenericPackageDescription
  -> ( PackageDescription
     , Maybe Version
     , [PackageFlag]
     , Maybe (DependencyTree Library)
     , [(UnqualComponentName, DependencyTree Library)]
     , [(UnqualComponentName, DependencyTree ForeignLib)]
     , [(UnqualComponentName, DependencyTree Executable)]
     , [(UnqualComponentName, DependencyTree TestSuite)]
     , [(UnqualComponentName, DependencyTree Benchmark)]
     )
viewGenericPackageDescription gpd =
  ( packageDescriptionInternal gpd
  , gpdScannedVersionInternal gpd
  , genPackageFlagsInternal gpd
  , condLibrary' gpd
  , condSubLibraries' gpd
  , condForeignLibs' gpd
  , condExecutables' gpd
  , condTestSuites' gpd
  , condBenchmarks' gpd
  )

libraryFromBuildInfo :: LibraryName -> BuildInfo -> Library
libraryFromBuildInfo n bi =
  emptyLibrary
    { libName = n
    , libVisibility = case n of
        LMainLibName -> LibraryVisibilityPublic
        LSubLibName _ -> LibraryVisibilityPrivate
    , libBuildInfo = bi
    }

foreignLibFromBuildInfo :: UnqualComponentName -> BuildInfo -> ForeignLib
foreignLibFromBuildInfo n bi = emptyForeignLib{foreignLibName = n, foreignLibBuildInfo = bi}

executableFromBuildInfo :: UnqualComponentName -> BuildInfo -> Executable
executableFromBuildInfo n bi = emptyExecutable{exeName = n, buildInfo = bi}

testSuiteStanzaFromBuildInfo :: BuildInfo -> TestSuiteStanza
testSuiteStanzaFromBuildInfo bi = TestSuiteStanza Nothing Nothing Nothing bi []

benchmarkStanzaFromBuildInfo :: BuildInfo -> BenchmarkStanza
benchmarkStanzaFromBuildInfo bi = BenchmarkStanza Nothing Nothing Nothing bi

condLibrary'
  :: GenericPackageDescription
  -> Maybe (DependencyTree Library)
condLibrary' gpd = mergeCondLibrary (gpdCommonStanzas gpd) <$> (condLibraryUnmerged gpd)

mergeCondLibrary
  :: Map ImportName (DependencyTree (WithImports BuildInfo))
  -> DependencyTree (WithImports Library)
  -> DependencyTree Library
mergeCondLibrary = flip mergeImports fromBuildInfo
  where
    fromBuildInfo = libraryFromBuildInfo . libName

condSubLibraries'
  :: GenericPackageDescription
  -> [(UnqualComponentName, DependencyTree Library)]
condSubLibraries' gpd = mergeCondSubLibraries (gpdCommonStanzas gpd) (condSubLibrariesUnmerged gpd)

mergeCondSubLibraries
  :: Map ImportName (DependencyTree (WithImports BuildInfo))
  -> [(UnqualComponentName, DependencyTree (WithImports Library))]
  -> [(UnqualComponentName, DependencyTree Library)]
mergeCondSubLibraries commonStanzas = map (mergeCondLibrary commonStanzas <$>)

condForeignLibs'
  :: GenericPackageDescription
  -> [(UnqualComponentName, DependencyTree ForeignLib)]
condForeignLibs' gpd = mergeCondForeignLibs (gpdCommonStanzas gpd) (condForeignLibsUnmerged gpd)

mergeCondForeignLibs
  :: Map ImportName (DependencyTree (WithImports BuildInfo))
  -> [(UnqualComponentName, DependencyTree (WithImports ForeignLib))]
  -> [(UnqualComponentName, DependencyTree ForeignLib)]
mergeCondForeignLibs commonStanzas = map $ \(name, tree) ->
  -- TODO(leana8959): is the name within the foreignlib important or we should use the name in the tuple?
  (name, mergeImports commonStanzas (const $ foreignLibFromBuildInfo name) tree)

condExecutables'
  :: GenericPackageDescription
  -> [(UnqualComponentName, DependencyTree Executable)]
condExecutables' gpd = mergeCondExecutables (gpdCommonStanzas gpd) (condExecutablesUnmerged gpd)

mergeCondExecutables
  :: Map ImportName (DependencyTree (WithImports BuildInfo))
  -> [(UnqualComponentName, DependencyTree (WithImports Executable))]
  -> [(UnqualComponentName, DependencyTree Executable)]
mergeCondExecutables commonStanzas = map $ \(name, tree) ->
  (name, mergeImports commonStanzas (const $ executableFromBuildInfo name) tree)

mergeTestSuiteStanza
  :: Map ImportName (DependencyTree (WithImports BuildInfo))
  -> DependencyTree (WithImports TestSuiteStanza)
  -> DependencyTree TestSuiteStanza
mergeTestSuiteStanza commonStanza =
  mergeImports commonStanza (const $ testSuiteStanzaFromBuildInfo)

mergeBenchmarkStanza
  :: Map ImportName (DependencyTree (WithImports BuildInfo))
  -> DependencyTree (WithImports BenchmarkStanza)
  -> DependencyTree BenchmarkStanza
mergeBenchmarkStanza commonStanza =
  mergeImports commonStanza (const $ benchmarkStanzaFromBuildInfo)

condTestSuites'
  :: GenericPackageDescription
  -> [(UnqualComponentName, DependencyTree TestSuite)]
condTestSuites' gpd =
  mergeTestSuiteStanza' (gpdCommonStanzas gpd) (condTestSuitesUnmerged gpd)
    & (map . fmap . mapTreeData) (convertTestSuite . patchTestSuiteType specVer)
  where
    specVer :: CabalSpecVersion
    specVer = specVersion . packageDescriptionInternal $ gpd

mergeTestSuiteStanza'
  :: Map ImportName (DependencyTree (WithImports BuildInfo))
  -> [(UnqualComponentName, DependencyTree (WithImports TestSuiteStanza))]
  -> [(UnqualComponentName, DependencyTree TestSuiteStanza)]
mergeTestSuiteStanza' commonStanza =
  map $
    fmap $
      mergeImports commonStanza (const $ testSuiteStanzaFromBuildInfo)

condBenchmarks'
  :: GenericPackageDescription
  -> [(UnqualComponentName, DependencyTree Benchmark)]
condBenchmarks' gpd =
  mergeBenchmarkStanza' (gpdCommonStanzas gpd) (condBenchmarksUnmerged gpd)
    & (map . fmap . mapTreeData) (convertBenchmark . patchBenchmarkType specVer)
  where
    specVer :: CabalSpecVersion
    specVer = specVersion . packageDescriptionInternal $ gpd

mergeBenchmarkStanza'
  :: Map ImportName (DependencyTree (WithImports BuildInfo))
  -> [(UnqualComponentName, DependencyTree (WithImports BenchmarkStanza))]
  -> [(UnqualComponentName, DependencyTree BenchmarkStanza)]
mergeBenchmarkStanza' commonStanza =
  map $
    fmap $
      mergeImports commonStanza (const $ benchmarkStanzaFromBuildInfo)

mergeImports
  :: forall a
   . L.HasBuildInfo a
  => Map ImportName (DependencyTree (WithImports BuildInfo))
  -> (a -> (BuildInfo -> a))
  -- ^ We need the information regarding the root node to be able to build such a constructor function
  -> DependencyTree (WithImports a)
  -> DependencyTree a
mergeImports commonStanzas fromBuildInfo (CondNode root c zs) =
  let endo :: DependencyTree a -> DependencyTree a
      endo = resolveImports (getImportNames root)

      tree :: DependencyTree a
      tree = CondNode (unImportNames root) c (map goBranch zs)
   in endo tree
  where
    goBranch
      :: L.HasBuildInfo a
      => CondBranch ConfVar [Dependency] (WithImports a)
      -> CondBranch ConfVar [Dependency] a
    goBranch (CondBranch cond ifTrue ifFalse) = CondBranch cond (goNode ifTrue) (goNode <$> ifFalse)
      where
        goNode = mergeImports commonStanzas fromBuildInfo

    resolveImports
      :: L.HasBuildInfo a
      => [ImportName]
      -> (DependencyTree a -> DependencyTree a)
    resolveImports importNames =
      let commonTrees :: [DependencyTree (WithImports BuildInfo)]
          commonTrees =
            map
              ( fromMaybe (error "failed to merge imports, did you mess with GenericPackageDescription?")
                  . flip Map.lookup commonStanzas
              )
              importNames

          commonTrees' :: [DependencyTree BuildInfo]
          commonTrees' = map goNode commonTrees
       in \x -> foldr mergeCondTree x commonTrees'
      where
        goNode = mergeImports commonStanzas (const id)

    mergeCondTree
      :: L.HasBuildInfo a
      => DependencyTree BuildInfo
      -> DependencyTree a
      -> DependencyTree a
    mergeCondTree (CondNode bi _ bis) (CondNode x _ cs) = CondNode x' (x' ^. L.targetBuildDepends) cs'
      where
        fromBuildInfo' :: (BuildInfo -> a)
        fromBuildInfo' = fromBuildInfo (unImportNames root)

        -- new value is old value with buildInfo field _prepended_.
        x' :: a
        x' = x & L.buildInfo %~ (bi <>)

        -- tree components are appended together.
        cs' :: [CondBranch ConfVar [Dependency] a]
        cs' = map (fromBuildInfo' <$>) bis ++ cs

instance Package GenericPackageDescription where
  packageId = packageId . packageDescription

instance Binary GenericPackageDescription
instance Structured GenericPackageDescription
instance NFData GenericPackageDescription where rnf = genericRnf

emptyGenericPackageDescription :: GenericPackageDescription
emptyGenericPackageDescription =
  GenericPackageDescription
    { packageDescription = emptyPackageDescription
    , gpdScannedVersion = Nothing
    , genPackageFlags = []
    , condLibrary = Nothing
    , condSubLibraries = []
    , condForeignLibs = []
    , condExecutables = []
    , condTestSuites = []
    , condBenchmarks = []
    }

-- -----------------------------------------------------------------------------
-- Traversal Instances

instance L.HasBuildInfos GenericPackageDescription where
  traverseBuildInfos f (GenericPackageDescription' p v a1 commonStanzas x1 x2 x3 x4 x5 x6) =
    GenericPackageDescription'
      <$> L.traverseBuildInfos f p
      <*> pure v
      <*> pure a1
      <*> (traverse . traverseCondTreeBuildInfo) f commonStanzas
      <*> (traverse . traverseCondTreeBuildInfo) f x1
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x2
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x3
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x4
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x5
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x6

-- We use this traversal to keep [Dependency] field in CondTree up to date.
traverseCondTreeBuildInfo
  :: forall f comp v
   . (Applicative f, L.HasBuildInfo comp)
  => LensLike' f (CondTree v [Dependency] comp) L.BuildInfo
traverseCondTreeBuildInfo g = node
  where
    mkCondNode :: comp -> [CondBranch v [Dependency] comp] -> CondTree v [Dependency] comp
    mkCondNode comp = CondNode comp (view L.targetBuildDepends comp)

    node (CondNode comp _ branches) =
      mkCondNode
        <$> L.buildInfo g comp
        <*> traverse branch branches

    branch (CondBranch v x y) =
      CondBranch v
        <$> node x
        <*> traverse node y
