{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Types.PackageSourceDescription (
    PackageSourceDescription(..),
    emptyPackageSourceDescription,
) where

import Prelude ()
import Distribution.Compat.Prelude

-- lens
import Distribution.Compat.Lens                     as L
import qualified Distribution.Types.BuildInfo.Lens  as L

import Distribution.Types.PackageDescription

import Distribution.Types.Benchmark
import Distribution.Types.CommonStanza
import Distribution.Types.CondTree
import Distribution.Types.ConfVar
import Distribution.Types.Dependency
import Distribution.Types.Executable
import Distribution.Types.Flag
import Distribution.Types.ForeignLib
import Distribution.Types.Library
import Distribution.Types.TestSuite
import Distribution.Types.UnqualComponentName
import Distribution.Package

-- ---------------------------------------------------------------------------
-- The 'PackageSourceDescription' type which represents the parsed package
-- source code before simplifications have been applied. This means that this
-- representation still contains comments and common stanzas.

data PackageSourceDescription =
  PackageSourceDescription
  { packageDescription :: PackageDescription
  , genPackageFlags    :: [Flag]
  , condCommonStanzas  :: [( UnqualComponentName
                           , CondTree ConfVar [Dependency] CommonStanza )]
  , condLibrary        :: Maybe (CondTree ConfVar [Dependency] Library)
  , condSubLibraries   :: [( UnqualComponentName
                           , CondTree ConfVar [Dependency] Library )]
  , condForeignLibs    :: [( UnqualComponentName
                           , CondTree ConfVar [Dependency] ForeignLib )]
  , condExecutables    :: [( UnqualComponentName
                           , CondTree ConfVar [Dependency] Executable )]
  , condTestSuites     :: [( UnqualComponentName
                           , CondTree ConfVar [Dependency] TestSuite )]
  , condBenchmarks     :: [( UnqualComponentName
                           , CondTree ConfVar [Dependency] Benchmark )]
  }
    deriving (Show, Eq, Typeable, Data, Generic)

instance Package PackageSourceDescription where
  packageId = packageId . packageDescription

instance Binary PackageSourceDescription
instance Structured PackageSourceDescription
instance NFData PackageSourceDescription where rnf = genericRnf

emptyPackageSourceDescription :: PackageSourceDescription
emptyPackageSourceDescription = PackageSourceDescription emptyPackageDescription [] [] Nothing [] [] [] [] []

-- -----------------------------------------------------------------------------
-- Traversal Instances

instance L.HasBuildInfos PackageSourceDescription where
  traverseBuildInfos f (PackageSourceDescription p a1 x1 x2 x3 x4 x5 x6 x7) =
    PackageSourceDescription
        <$> L.traverseBuildInfos f p
        <*> pure a1
        <*> (traverse . L._2 . traverse . L.buildInfo) f x1
        <*> (traverse . traverse . L.buildInfo) f x2
        <*> (traverse . L._2 . traverse . L.buildInfo) f x3
        <*> (traverse . L._2 . traverse . L.buildInfo) f x4
        <*> (traverse . L._2 . traverse . L.buildInfo) f x5
        <*> (traverse . L._2 . traverse . L.buildInfo) f x6
        <*> (traverse . L._2 . traverse . L.buildInfo) f x7
