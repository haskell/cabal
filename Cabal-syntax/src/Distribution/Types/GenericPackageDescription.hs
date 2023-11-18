{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Types.GenericPackageDescription
  ( GenericPackageDescription (..)
  , ExactPrintMeta(..)
  , ExactPosition(..)
  , NameSpace(..)
  , emptyGenericPackageDescription
  , emptyExactPrintMeta
  ) where

import Distribution.Compat.Prelude
import Prelude ()

-- lens
import Distribution.Compat.Lens as L
import qualified Distribution.Types.BuildInfo.Lens as L

import Distribution.Types.PackageDescription

import Distribution.Package
import Distribution.Types.Benchmark
import Distribution.Types.CondTree
import Distribution.Types.ConfVar
import Distribution.Types.Executable
import Distribution.Types.Flag
import Distribution.Types.ForeignLib
import Distribution.Types.Library
import Distribution.Types.TestSuite
import Distribution.Types.UnqualComponentName
import Distribution.Version
import Data.Text(Text)
import Distribution.Fields.Field(FieldName)
import Distribution.Parsec.Position(Position)
import Data.ByteString(ByteString)

data ExactPosition = ExactPosition {namePosition :: Position
                                -- argument can be filedline or section args
                                -- recursive names within sections have their own
                                -- name identifier so they're not modelled
                                , argumentPosition :: [Position] }
  deriving (Show, Eq, Typeable, Data, Generic, Ord)
instance Structured ExactPosition
instance NFData ExactPosition where rnf = genericRnf
instance Binary ExactPosition

-- | we need to distinct exact positions in various namespaces for fields,
--  such as:
-- @
--  library:
--      build-depends: base < 4
--   ...
--   executable two
--      build-depends: base <5
--                     , containers > 3
--   executable three
--      build-depends: base <5
--                     , containers > 5
-- @
--  so we put "exectuabe" or "library" as field name
--  and the arguments such as "two" and "three" as section argument.
--  this allows us to distinct them in the 'exactPositions'
data NameSpace = NameSpace
  { nameSpaceName :: FieldName
  , nameSpaceSectionArgs :: [ByteString]
  }
  deriving (Show, Eq, Typeable, Data, Ord, Generic)

instance Binary NameSpace
instance Structured NameSpace
instance NFData NameSpace where rnf = genericRnf

data ExactPrintMeta = ExactPrintMeta
  { exactPositions :: Map [NameSpace] ExactPosition
  , exactComments :: Map Position Text
  }
  deriving (Show, Eq, Typeable, Data, Generic)

instance Binary ExactPrintMeta
instance Structured ExactPrintMeta
instance NFData ExactPrintMeta where rnf = genericRnf

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
  , condLibrary :: Maybe (CondTree ConfVar [Dependency] Library)
  , condSubLibraries
      :: [ ( UnqualComponentName
           , CondTree ConfVar [Dependency] Library
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
  , exactPrintMeta :: ExactPrintMeta
  }
  deriving (Show, Eq, Typeable, Data, Generic)


instance Package GenericPackageDescription where
  packageId = packageId . packageDescription

instance Structured GenericPackageDescription

-- | Required for rebuild monad
instance Binary GenericPackageDescription
instance NFData GenericPackageDescription where rnf = genericRnf

emptyExactPrintMeta :: ExactPrintMeta
emptyExactPrintMeta = ExactPrintMeta mempty mempty

emptyGenericPackageDescription :: GenericPackageDescription
emptyGenericPackageDescription = GenericPackageDescription
  { packageDescription = emptyPackageDescription
  , gpdScannedVersion = Nothing
  , genPackageFlags = []
  , condLibrary =  Nothing
  , condSubLibraries = []
  , condForeignLibs = []
  , condExecutables = []
  , condTestSuites = []
  , condBenchmarks = []
  , exactPrintMeta = emptyExactPrintMeta
  }

-- -----------------------------------------------------------------------------
-- Traversal Instances

instance L.HasBuildInfos GenericPackageDescription where
  traverseBuildInfos f (GenericPackageDescription p v a1 x1 x2 x3 x4 x5 x6 exactPrintMeta') =
    GenericPackageDescription
      <$> L.traverseBuildInfos f p
      <*> pure v
      <*> pure a1
      <*> (traverse . traverseCondTreeBuildInfo) f x1
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x2
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x3
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x4
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x5
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x6
      <*> pure exactPrintMeta'

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

