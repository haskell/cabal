{-# LANGUAGE DeriveGeneric #-}

-- | Per-package constraints. Package constraints must be respected by the
-- solver. Multiple constraints for each package can be given, though obviously
-- it is possible to construct conflicting constraints (eg impossible version
-- range or inconsistent flag assignment).
--
module Distribution.Solver.Types.PackageConstraint (
    ConstraintScope(..),
    scopeToplevel,
    scopeToPackageName,
    PackageProperty(..),
    dispPackageProperty,
    PackageConstraint(..),
    dispPackageConstraint,
    showPackageConstraint,
    packageConstraintToDependency
  ) where

import Distribution.Compat.Binary      (Binary(..))
import Distribution.Package            (PackageName)
import Distribution.PackageDescription (FlagAssignment, dispFlagAssignment)
import Distribution.Types.Dependency   (Dependency(..))
import Distribution.Version            (VersionRange, simplifyVersionRange)

import Distribution.Client.Compat.Prelude ((<<>>))
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackagePath

import Distribution.Text                  (disp, flatStyle)
import GHC.Generics                       (Generic)
import Text.PrettyPrint                   ((<+>))
import qualified Text.PrettyPrint as Disp


-- | Determines to what packages and in what contexts a
-- constraint applies.
data ConstraintScope
     -- | The package with the specified name and qualifier.
   = ScopeQualified Qualifier PackageName
     -- | The package with the specified name regardless of
     -- qualifier.
   | ScopeAnyQualifier PackageName
  deriving (Eq, Show)

-- | Constructor for a common use case: the constraint applies to
-- the package with the specified name when that package is a
-- top-level dependency in the default namespace.
scopeToplevel :: PackageName -> ConstraintScope
scopeToplevel = ScopeQualified QualToplevel

-- | Returns the package name associated with a constraint scope.
scopeToPackageName :: ConstraintScope -> PackageName
scopeToPackageName (ScopeQualified _ pn) = pn
scopeToPackageName (ScopeAnyQualifier pn) = pn

-- | Pretty-prints a constraint scope.
dispConstraintScope :: ConstraintScope -> Disp.Doc
dispConstraintScope (ScopeQualified q pn) = dispQualifier q <<>> disp pn
dispConstraintScope (ScopeAnyQualifier pn) = Disp.text "any." <<>> disp pn

-- | A package property is a logical predicate on packages.
data PackageProperty
   = PackagePropertyVersion   VersionRange
   | PackagePropertyInstalled
   | PackagePropertySource
   | PackagePropertyFlags     FlagAssignment
   | PackagePropertyStanzas   [OptionalStanza]
  deriving (Eq, Show, Generic)

instance Binary PackageProperty

-- | Pretty-prints a package property.
dispPackageProperty :: PackageProperty -> Disp.Doc
dispPackageProperty (PackagePropertyVersion verrange) = disp verrange
dispPackageProperty PackagePropertyInstalled          = Disp.text "installed"
dispPackageProperty PackagePropertySource             = Disp.text "source"
dispPackageProperty (PackagePropertyFlags flags)      = dispFlagAssignment flags
dispPackageProperty (PackagePropertyStanzas stanzas)  =
  Disp.hsep $ map (Disp.text . showStanza) stanzas

-- | A package constraint consists of a scope plus a property
-- that must hold for all packages within that scope.
data PackageConstraint = PackageConstraint ConstraintScope PackageProperty
  deriving (Eq, Show)

-- | Pretty-prints a package constraint.
dispPackageConstraint :: PackageConstraint -> Disp.Doc
dispPackageConstraint (PackageConstraint scope prop) =
  dispConstraintScope scope <+> dispPackageProperty prop

-- | Alternative textual representation of a package constraint
-- for debugging purposes (slightly more verbose than that
-- produced by 'dispPackageConstraint').
--
showPackageConstraint :: PackageConstraint -> String
showPackageConstraint pc@(PackageConstraint scope prop) =
  Disp.renderStyle flatStyle . postprocess $ dispPackageConstraint pc2
  where
    pc2 = case prop of
      PackagePropertyVersion vr ->
        PackageConstraint scope $ PackagePropertyVersion (simplifyVersionRange vr)
      _ -> pc
    postprocess = case prop of
      PackagePropertyFlags _ -> (Disp.text "flags" <+>)
      PackagePropertyStanzas _ -> (Disp.text "stanzas" <+>)
      _ -> id

-- | Lossily convert a 'PackageConstraint' to a 'Dependency'.
packageConstraintToDependency :: PackageConstraint -> Maybe Dependency
packageConstraintToDependency (PackageConstraint scope prop) = toDep prop
  where
    toDep (PackagePropertyVersion vr) = 
        Just $ Dependency (scopeToPackageName scope) vr
    toDep (PackagePropertyInstalled)  = Nothing
    toDep (PackagePropertySource)     = Nothing
    toDep (PackagePropertyFlags _)    = Nothing
    toDep (PackagePropertyStanzas _)  = Nothing
