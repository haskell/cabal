{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Types.ConfiguredId
  ( InstalledPackageId
  , ConfiguredId (..)
  , annotatedIdToConfiguredId
  , HasConfiguredId (..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.InstalledPackageInfo (InstalledPackageInfo, installedComponentId, sourceComponentName)
import Distribution.Package (Package (..))
import Distribution.Types.AnnotatedId (AnnotatedId (..))
import Distribution.Types.ComponentId (ComponentId)
import Distribution.Types.ComponentName (ComponentName)
import Distribution.Types.PackageId (PackageId)

-------------------------------------------------------------------------------
-- InstalledPackageId
-------------------------------------------------------------------------------

-- | Within Cabal the library we no longer have a @InstalledPackageId@ type.
-- That's because it deals with the compilers' notion of a registered library,
-- and those really are libraries not packages. Those are now named units.
--
-- The package management layer does however deal with installed packages, as
-- whole packages not just as libraries. So we do still need a type for
-- installed package ids. At the moment however we track installed packages via
-- their primary library, which is a unit id. In future this may change
-- slightly and we may distinguish these two types and have an explicit
-- conversion when we register units with the compiler.
type InstalledPackageId = ComponentId

-------------------------------------------------------------------------------
-- ConfiguredId
-------------------------------------------------------------------------------

-- | A ConfiguredId is a package ID for a configured package.
--
-- Once we configure a source package we know its UnitId. It is still
-- however useful in lots of places to also know the source ID for the package.
-- We therefore bundle the two.
--
-- An already installed package of course is also "configured" (all its
-- configuration parameters and dependencies have been specified).
data ConfiguredId = ConfiguredId
  { confSrcId :: PackageId
  , confCompName :: Maybe ComponentName
  , confInstId :: ComponentId
  }
  deriving (Eq, Ord, Generic)

annotatedIdToConfiguredId :: AnnotatedId ComponentId -> ConfiguredId
annotatedIdToConfiguredId aid =
  ConfiguredId
    { confSrcId = ann_pid aid
    , confCompName = Just (ann_cname aid)
    , confInstId = ann_id aid
    }

instance Binary ConfiguredId
instance Structured ConfiguredId

instance Show ConfiguredId where
  show cid = show (confInstId cid)

instance Package ConfiguredId where
  packageId = confSrcId

-------------------------------------------------------------------------------
-- HasConfiguredId class
-------------------------------------------------------------------------------

class HasConfiguredId a where
  configuredId :: a -> ConfiguredId

-- NB: This instance is slightly dangerous, in that you'll lose
-- information about the specific UnitId you depended on.
instance HasConfiguredId InstalledPackageInfo where
  configuredId ipkg =
    ConfiguredId
      (packageId ipkg)
      (Just (sourceComponentName ipkg))
      (installedComponentId ipkg)
