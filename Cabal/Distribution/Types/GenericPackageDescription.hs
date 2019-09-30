{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Types.GenericPackageDescription (
    GenericPackageDescription(..),
    emptyGenericPackageDescription,
) where

import Prelude ()
import Distribution.Compat.Prelude

-- lens
import Distribution.Compat.Lens                     as L
import qualified Distribution.Types.BuildInfo.Lens  as L

import Distribution.Types.PackageDescription

import Distribution.Types.Benchmark
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
-- The 'GenericPackageDescription' type

data GenericPackageDescription =
  GenericPackageDescription
  { packageDescription :: PackageDescription
  , genPackageFlags    :: [Flag]
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

instance Package GenericPackageDescription where
  packageId = packageId . packageDescription

instance Binary GenericPackageDescription

instance NFData GenericPackageDescription where rnf = genericRnf

emptyGenericPackageDescription :: GenericPackageDescription
emptyGenericPackageDescription = GenericPackageDescription emptyPackageDescription [] Nothing [] [] [] [] []

-- -----------------------------------------------------------------------------
-- Traversal Instances

instance L.HasBuildInfos GenericPackageDescription where
  traverseBuildInfos f (GenericPackageDescription p a1 x1 x2 x3 x4 x5 x6) =
    GenericPackageDescription
        <$> L.traverseBuildInfos f p
        <*> pure a1
        <*> (traverse . traverse . L.buildInfo) f x1
        <*> (traverse . L._2 . traverse . L.buildInfo) f x2
        <*> (traverse . L._2 . traverse . L.buildInfo) f x3
        <*> (traverse . L._2 . traverse . L.buildInfo) f x4
        <*> (traverse . L._2 . traverse . L.buildInfo) f x5
        <*> (traverse . L._2 . traverse . L.buildInfo) f x6


