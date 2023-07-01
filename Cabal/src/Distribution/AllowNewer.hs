-----------------------------------------------------------------------------

-- | Utilities to relax version bounds on dependencies
module Distribution.AllowNewer
  ( relaxPackageDeps
  , RelaxKind (..)
  ) where

import Distribution.Compat.Prelude

import Distribution.Package
  ( Package (..)
  , packageName
  )
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Configuration as PD
import Distribution.Types.Dependency
import Distribution.Version

import qualified Data.Map as Map
import Distribution.Types.AllowNewer

data RelaxKind = RelaxLower | RelaxUpper

-- | Relax the dependencies of this package if needed.
--
-- Helper function used by 'removeBounds'
relaxPackageDeps
  :: RelaxKind
  -> RelaxDeps
  -> PD.GenericPackageDescription
  -> PD.GenericPackageDescription
relaxPackageDeps _ rd gpd | not (isRelaxDeps rd) = gpd -- subsumed by no-op case in 'removeBounds'
relaxPackageDeps relKind RelaxDepsAll gpd = PD.transformAllBuildDepends relaxAll gpd
  where
    relaxAll :: Dependency -> Dependency
    relaxAll (Dependency pkgName verRange cs) =
      Dependency pkgName (removeBound relKind RelaxDepModNone verRange) cs
relaxPackageDeps relKind (RelaxDepsSome depsToRelax0) gpd =
  PD.transformAllBuildDepends relaxSome gpd
  where
    thisPkgName = packageName gpd
    thisPkgId = packageId gpd
    depsToRelax = Map.fromList $ mapMaybe f depsToRelax0

    f :: RelaxedDep -> Maybe (RelaxDepSubject, RelaxDepMod)
    f (RelaxedDep scope rdm p) = case scope of
      RelaxDepScopeAll -> Just (p, rdm)
      RelaxDepScopePackage p0
        | p0 == thisPkgName -> Just (p, rdm)
        | otherwise -> Nothing
      RelaxDepScopePackageId p0
        | p0 == thisPkgId -> Just (p, rdm)
        | otherwise -> Nothing

    relaxSome :: Dependency -> Dependency
    relaxSome d@(Dependency depName verRange cs)
      | Just relMod <- Map.lookup RelaxDepSubjectAll depsToRelax =
          -- a '*'-subject acts absorbing, for consistency with
          -- the 'Semigroup RelaxDeps' instance
          Dependency depName (removeBound relKind relMod verRange) cs
      | Just relMod <- Map.lookup (RelaxDepSubjectPkg depName) depsToRelax =
          Dependency depName (removeBound relKind relMod verRange) cs
      | otherwise = d -- no-op

-- | Internal helper for 'relaxPackageDeps'
removeBound :: RelaxKind -> RelaxDepMod -> VersionRange -> VersionRange
removeBound RelaxLower RelaxDepModNone = removeLowerBound
removeBound RelaxUpper RelaxDepModNone = removeUpperBound
removeBound RelaxLower RelaxDepModCaret = transformCaretLower
removeBound RelaxUpper RelaxDepModCaret = transformCaretUpper
