{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Types.GenericPackageDescription
  ( GenericPackageDescription
  , GenericPackageDescriptionAnn
  , GenericPackageDescriptionWith (..)
  , emptyGenericPackageDescription
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

import Data.Kind
import qualified Distribution.Types.Modify as Mod

-- ---------------------------------------------------------------------------
-- The 'GenericPackageDescription' type

type GenericPackageDescription = GenericPackageDescriptionWith Mod.HasNoAnn
type GenericPackageDescriptionAnn = GenericPackageDescriptionWith Mod.HasAnn

data GenericPackageDescriptionWith (m :: Mod.HasAnnotation) = GenericPackageDescription
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
  , condLibrary :: (Maybe (CondTree ConfVar (LibraryWith m)))
  , condSubLibraries
      :: [(UnqualComponentName, CondTree ConfVar (LibraryWith m))]
  , condForeignLibs
      :: [(UnqualComponentName, CondTree ConfVar (ForeignLibWith m))]
  , condExecutables
      :: [(UnqualComponentName, CondTree ConfVar (ExecutableWith m))]
  , condTestSuites
      :: [(UnqualComponentName, CondTree ConfVar (TestSuiteWith m))]
  , condBenchmarks
      :: [(UnqualComponentName, CondTree ConfVar (BenchmarkWith m))]
  }

deriving instance Eq (GenericPackageDescriptionWith Mod.HasNoAnn)
deriving instance Show (GenericPackageDescriptionWith Mod.HasNoAnn)
deriving instance Data (GenericPackageDescriptionWith Mod.HasNoAnn)
deriving instance Generic (GenericPackageDescriptionWith Mod.HasNoAnn)

deriving instance Show (GenericPackageDescriptionWith Mod.HasAnn)

instance Package (GenericPackageDescriptionWith Mod.HasNoAnn) where
  packageId = packageId . packageDescription

deriving instance Binary (GenericPackageDescriptionWith Mod.HasNoAnn)

instance Structured GenericPackageDescription
instance NFData GenericPackageDescription where rnf = genericRnf

emptyGenericPackageDescription :: GenericPackageDescriptionWith mod
emptyGenericPackageDescription = GenericPackageDescription emptyPackageDescription Nothing [] Nothing [] [] [] [] []

-- -----------------------------------------------------------------------------
-- Traversal Instances

instance L.HasBuildInfosWith Mod.HasNoAnn GenericPackageDescription where
  traverseBuildInfos f (GenericPackageDescription p v a1 x1 x2 x3 x4 x5 x6) =
    GenericPackageDescription
      <$> L.traverseBuildInfos f p
      <*> pure v
      <*> pure a1
      <*> (traverse . traverse . L.buildInfo) f x1
      <*> (traverse . L._2 . traverse . L.buildInfo) f x2
      <*> (traverse . L._2 . traverse . L.buildInfo) f x3
      <*> (traverse . L._2 . traverse . L.buildInfo) f x4
      <*> (traverse . L._2 . traverse . L.buildInfo) f x5
      <*> (traverse . L._2 . traverse . L.buildInfo) f x6
