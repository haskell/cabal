{-# LANGUAGE Rank2Types #-}

module Distribution.Types.GenericPackageDescription.Lens
  ( GenericPackageDescription
  , PackageFlag
  , FlagName
  , ConfVar (..)
  , module Distribution.Types.GenericPackageDescription.Lens
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import qualified Distribution.Types.GenericPackageDescription as T
import qualified Distribution.Types.Imports as T

-- We import types from their packages, so we can remove unused imports
-- and have wider inter-module dependency graph

import Distribution.Compiler (CompilerFlavor)
import Distribution.System (Arch, OS)
import Distribution.Types.BenchmarkStanza (BenchmarkStanza)
import Distribution.Types.BuildInfo (BuildInfo)
import Distribution.Types.CondTree (CondTree)
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Executable (Executable)
import Distribution.Types.Flag (FlagName, PackageFlag (MkPackageFlag))
import Distribution.Types.ForeignLib (ForeignLib)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Imports (ImportName)
import Distribution.Types.Library (Library)
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Types.TestSuiteStanza (TestSuiteStanza)
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import Distribution.Version (Version, VersionRange)

-------------------------------------------------------------------------------
-- GenericPackageDescription
-------------------------------------------------------------------------------

packageDescription :: Lens' GenericPackageDescription PackageDescription
packageDescription f s = fmap (\x -> s{T.packageDescription = x}) (f (T.packageDescription s))
{-# INLINE packageDescription #-}

gpdScannedVersion :: Lens' GenericPackageDescription (Maybe Version)
gpdScannedVersion f s = fmap (\x -> s{T.gpdScannedVersion = x}) (f (T.gpdScannedVersion s))
{-# INLINE gpdScannedVersion #-}

genPackageFlags :: Lens' GenericPackageDescription [PackageFlag]
genPackageFlags f s = fmap (\x -> s{T.genPackageFlags = x}) (f (T.genPackageFlags s))
{-# INLINE genPackageFlags #-}

gpdCommonStanzas :: Lens' GenericPackageDescription (Map ImportName (CondTree ConfVar [Dependency] (T.WithImports BuildInfo)))
gpdCommonStanzas f s = fmap (\x -> s{T.gpdCommonStanzas = x}) (f (T.gpdCommonStanzas s))
{-# INLINE gpdCommonStanzas #-}

-- TODO(leana8959): how to deal with this
condLibraryUnmerged :: Lens' GenericPackageDescription (Maybe (CondTree ConfVar [Dependency] (T.WithImports Library)))
condLibraryUnmerged f s = fmap (\x -> s{T.condLibraryUnmerged = x}) (f (T.condLibraryUnmerged s))
{-# INLINE condLibraryUnmerged #-}

condSubLibrariesUnmerged :: Lens' GenericPackageDescription [(UnqualComponentName, (CondTree ConfVar [Dependency] (T.WithImports Library)))]
condSubLibrariesUnmerged f s = fmap (\x -> s{T.condSubLibrariesUnmerged = x}) (f (T.condSubLibrariesUnmerged s))
{-# INLINE condSubLibrariesUnmerged #-}

condForeignLibsUnmerged :: Lens' GenericPackageDescription [(UnqualComponentName, (CondTree ConfVar [Dependency] (T.WithImports ForeignLib)))]
condForeignLibsUnmerged f s = fmap (\x -> s{T.condForeignLibsUnmerged = x}) (f (T.condForeignLibsUnmerged s))
{-# INLINE condForeignLibsUnmerged #-}

condExecutablesUnmerged :: Lens' GenericPackageDescription [(UnqualComponentName, (CondTree ConfVar [Dependency] (T.WithImports Executable)))]
condExecutablesUnmerged f s = fmap (\x -> s{T.condExecutablesUnmerged = x}) (f (T.condExecutablesUnmerged s))
{-# INLINE condExecutablesUnmerged #-}

condTestSuitesUnmerged :: Lens' GenericPackageDescription [(UnqualComponentName, (CondTree ConfVar [Dependency] (T.WithImports TestSuiteStanza)))]
condTestSuitesUnmerged f s = fmap (\x -> s{T.condTestSuitesUnmerged = x}) (f (T.condTestSuitesUnmerged s))
{-# INLINE condTestSuitesUnmerged #-}

condBenchmarksUnmerged :: Lens' GenericPackageDescription [(UnqualComponentName, (CondTree ConfVar [Dependency] (T.WithImports BenchmarkStanza)))]
condBenchmarksUnmerged f s = fmap (\x -> s{T.condBenchmarksUnmerged = x}) (f (T.condBenchmarksUnmerged s))
{-# INLINE condBenchmarksUnmerged #-}

allCondTrees
  :: Applicative f
  => ( forall a
        . CondTree ConfVar [Dependency] a
       -> f (CondTree ConfVar [Dependency] a)
     )
  -> GenericPackageDescription
  -> f GenericPackageDescription
allCondTrees f (GenericPackageDescription' p v a1 commonStanzas x1 x2 x3 x4 x5 x6) =
  GenericPackageDescription'
    <$> pure p
    <*> pure v
    <*> pure a1
    <*> traverse f commonStanzas
    <*> traverse f x1
    <*> (traverse . _2) f x2
    <*> (traverse . _2) f x3
    <*> (traverse . _2) f x4
    <*> (traverse . _2) f x5
    <*> (traverse . _2) f x6

-------------------------------------------------------------------------------
-- Flag
-------------------------------------------------------------------------------

flagName :: Lens' PackageFlag FlagName
flagName f (MkPackageFlag x1 x2 x3 x4) = fmap (\y1 -> MkPackageFlag y1 x2 x3 x4) (f x1)
{-# INLINE flagName #-}

flagDescription :: Lens' PackageFlag String
flagDescription f (MkPackageFlag x1 x2 x3 x4) = fmap (\y1 -> MkPackageFlag x1 y1 x3 x4) (f x2)
{-# INLINE flagDescription #-}

flagDefault :: Lens' PackageFlag Bool
flagDefault f (MkPackageFlag x1 x2 x3 x4) = fmap (\y1 -> MkPackageFlag x1 x2 y1 x4) (f x3)
{-# INLINE flagDefault #-}

flagManual :: Lens' PackageFlag Bool
flagManual f (MkPackageFlag x1 x2 x3 x4) = fmap (\y1 -> MkPackageFlag x1 x2 x3 y1) (f x4)
{-# INLINE flagManual #-}

-------------------------------------------------------------------------------
-- ConfVar
-------------------------------------------------------------------------------

_OS :: Traversal' ConfVar OS
_OS f (OS os) = OS <$> f os
_OS _ x = pure x

_Arch :: Traversal' ConfVar Arch
_Arch f (Arch arch) = Arch <$> f arch
_Arch _ x = pure x

_PackageFlag :: Traversal' ConfVar FlagName
_PackageFlag f (PackageFlag flag) = PackageFlag <$> f flag
_PackageFlag _ x = pure x

_Impl :: Traversal' ConfVar (CompilerFlavor, VersionRange)
_Impl f (Impl cf vr) = uncurry Impl <$> f (cf, vr)
_Impl _ x = pure x
