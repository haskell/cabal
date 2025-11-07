{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Types.GenericPackageDescription
  ( GenericPackageDescription (..)
  , emptyGenericPackageDescription

  -- TODO(leana8959): rename this
  , _condLibrary
  , condSubLibraries'
  , mergeCondSubLibraries
  ) where

import Distribution.Compat.Prelude
import Prelude ()

-- lens
import Distribution.Compat.Lens as L
import qualified Distribution.Types.BuildInfo.Lens as L

-- TODO(leana8959): fix it this orphan
import qualified Distribution.Types.Imports.Lens as L ()

import Distribution.Types.PackageDescription

import Distribution.Package
import Distribution.Types.Benchmark
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
import Distribution.Types.UnqualComponentName
import Distribution.Version

import qualified Data.Map as Map

-- ---------------------------------------------------------------------------
-- The 'GenericPackageDescription' type

data GenericPackageDescription = GenericPackageDescription
  { packageDescription :: PackageDescription
  , gpdScannedVersion :: Maybe Version
  -- ^ This is a version as specified in source.
  --   We populate this field in index reading for dummy GPDs,
  --   only when GPD reading failed, but scanning haven't.
  --
  --   Cabal-the-library never produces GPDs with Just as gpdScannedVersion.
  --
  --   Perfectly, PackageIndex should have sum type, so we don't need to
  --   have dummy GPDs.
  , genPackageFlags :: [PackageFlag]
  , gpdCommonStanzas :: Map ImportName (CondTree ConfVar [Dependency] (WithImports BuildInfo))
  , condLibrary :: Maybe (CondTree ConfVar [Dependency] (WithImports Library))
  , condSubLibraries
      :: [ ( UnqualComponentName
           , CondTree ConfVar [Dependency] (WithImports Library)
           )
         ]
  , condForeignLibs
      :: [ ( UnqualComponentName
           , CondTree ConfVar [Dependency] ForeignLib
           )
         ]
  , condExecutables
      :: [ ( UnqualComponentName
           , CondTree ConfVar [Dependency] Executable
           )
         ]
  , condTestSuites
      :: [ ( UnqualComponentName
           , CondTree ConfVar [Dependency] TestSuite
           )
         ]
  , condBenchmarks
      :: [ ( UnqualComponentName
           , CondTree ConfVar [Dependency] Benchmark
           )
         ]
  }
  deriving (Show, Eq, Data, Generic)

libraryFromBuildInfo :: LibraryName -> BuildInfo -> Library
libraryFromBuildInfo n bi =
  emptyLibrary
    { libName = n
    , libVisibility = case n of
        LMainLibName -> LibraryVisibilityPublic
        LSubLibName _ -> LibraryVisibilityPrivate
    , libBuildInfo = bi
    }

_condLibrary :: GenericPackageDescription -> Maybe (CondTree ConfVar [Dependency] Library)
_condLibrary gpd =
  let commonStanzas = gpdCommonStanzas gpd
      fromBuildInfo :: Library -> (BuildInfo -> Library)
      fromBuildInfo = libraryFromBuildInfo . libName
   in mergeImports commonStanzas fromBuildInfo <$> (condLibrary gpd)

condSubLibraries' :: GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] Library)]
condSubLibraries' gpd = mergeCondSubLibraries (gpdCommonStanzas gpd) (condSubLibraries gpd)

mergeCondSubLibraries
  :: Map ImportName (CondTree ConfVar [Dependency] (WithImports BuildInfo))
  -> [(UnqualComponentName, CondTree ConfVar [Dependency] (WithImports Library))]
  -> [(UnqualComponentName, CondTree ConfVar [Dependency] Library)]
mergeCondSubLibraries commonStanzas = map (fmap go)
  where
    go :: CondTree ConfVar [Dependency] (WithImports Library) -> CondTree ConfVar [Dependency] Library
    go lib =
      let fromBuildInfo :: Library -> (BuildInfo -> Library)
          fromBuildInfo = libraryFromBuildInfo . libName
      in  mergeImports commonStanzas fromBuildInfo lib

mergeImports
  :: forall a
   . L.HasBuildInfo a
  => Map ImportName (CondTree ConfVar [Dependency] (WithImports BuildInfo))
  -> (a -> (BuildInfo -> a))
  -- ^ We need the information regarding the root node to be able to build such a constructor function
  -> CondTree ConfVar [Dependency] (WithImports a)
  -> CondTree ConfVar [Dependency] a
mergeImports commonStanzas fromBuildInfo (CondNode root c zs) =
  let endo :: CondTree ConfVar [Dependency] a -> CondTree ConfVar [Dependency] a
      endo = resolveImports (getImportNames root)

      tree :: CondTree ConfVar [Dependency] a
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

    -- TODO(leana8959): (at some point it'll become (WithImports BuildInfo) and it'll force us to handle its imports too)
    resolveImports
      :: L.HasBuildInfo a
      => [ImportName]
      -> (CondTree ConfVar [Dependency] a -> CondTree ConfVar [Dependency] a)
    resolveImports importNames =
      let commonTrees :: [CondTree ConfVar [Dependency] (WithImports BuildInfo)]
          commonTrees =
            map
              ( fromMaybe (error "failed to merge imports, did you mess with GenericPackageDescription?")
                  . flip Map.lookup commonStanzas
              )
              importNames

          commonTrees' :: [CondTree ConfVar [Dependency] BuildInfo]
          commonTrees' = map goNode commonTrees
       in \x -> foldr mergeCondTree x commonTrees'
      where
        goNode = mergeImports commonStanzas (const id)

    mergeCondTree
      :: L.HasBuildInfo a
      => CondTree ConfVar [Dependency] BuildInfo
      -> CondTree ConfVar [Dependency] a
      -> CondTree ConfVar [Dependency] a
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

-- condSubLibraries :: GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] Library)]
-- condSubLibraries = ()
-- condForeignLibs :: GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)]
-- condForeignLibs = ()
-- condExecutables :: GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)]
-- condExecutables = ()
-- condTestSuites :: GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)]
-- condTestSuites = ()
-- condBenchmarks :: GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)]
-- condBenchmarks = ()

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
    , gpdCommonStanzas = mempty
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
  traverseBuildInfos f (GenericPackageDescription p v a1 commonStanzas x1 x2 x3 x4 x5 x6) =
    GenericPackageDescription
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
