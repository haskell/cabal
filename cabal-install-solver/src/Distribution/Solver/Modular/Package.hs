{-# LANGUAGE DeriveFunctor #-}
module Distribution.Solver.Modular.Package
  ( I(..)
  , Loc(..)
  , PackageId
  , PackageIdentifier(..)
  , PackageName, mkPackageName, unPackageName
  , PkgconfigName, mkPkgconfigName, unPkgconfigName
  , PI(..)
  , PN
  , QPV
  , instI
  , makeIndependent
  , showI
  , showPI
  , unPN
  ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude

import Distribution.Package -- from Cabal
import Distribution.Pretty (prettyShow)

import Distribution.Solver.Modular.Version
import Distribution.Solver.Types.PackagePath

-- | A package name.
type PN = PackageName

-- | Unpacking a package name.
unPN :: PN -> String
unPN = unPackageName

-- | Package version. A package name plus a version number.
type PV = PackageId

-- | Qualified package version.
type QPV = Qualified PV

-- | Package id. Currently just a black-box string.
type PId = UnitId

-- | Location. Info about whether a package is installed or not, and where
-- exactly it is located. For installed packages, uniquely identifies the
-- package instance via its 'PId'.
--
-- TODO: More information is needed about the repo.
data Loc = Inst PId | InRepo
  deriving (Eq, Ord, Show)

-- | Instance. A version number and a location.
data I = I Ver Loc
  deriving (Eq, Ord, Show)

-- | String representation of an instance.
showI :: I -> String
showI (I v InRepo)   = showVer v
showI (I v (Inst uid)) = showVer v ++ "/installed" ++ extractPackageAbiHash uid
  where
    extractPackageAbiHash xs =
      case first reverse $ break (=='-') $ reverse (prettyShow xs) of
        (ys, []) -> ys
        (ys, _)  -> '-' : ys

-- | Package instance. A package name and an instance.
data PI qpn = PI qpn I
  deriving (Eq, Ord, Show, Functor)

-- | String representation of a package instance.
showPI :: PI QPN -> String
showPI (PI qpn i) = showQPN qpn ++ "-" ++ showI i

instI :: I -> Bool
instI (I _ (Inst _)) = True
instI _              = False

-- | Qualify a target package with its own name so that its dependencies are not
-- required to be consistent with other targets.
makeIndependent :: PN -> QPN
makeIndependent pn = Q (PackagePath (Independent pn) QualToplevel) pn
