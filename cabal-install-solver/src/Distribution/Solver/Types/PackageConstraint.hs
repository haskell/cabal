{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- | Per-package constraints. Package constraints must be respected by the
-- solver. Multiple constraints for each package can be given, though obviously
-- it is possible to construct conflicting constraints (eg impossible version
-- range or inconsistent flag assignment).
--
module Distribution.Solver.Types.PackageConstraint (
    ConstraintScope(..),
    scopeToplevel,
    scopeToPackageName,
    constraintScopeMatches,
    PackageProperty(..),
    dispPackageProperty,
    PackageConstraint(..),
    dispPackageConstraint,
    showPackageConstraint,
    packageConstraintToDependency
  ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Package                        (PackageName)
import Distribution.PackageDescription             (FlagAssignment, dispFlagAssignment)
import Distribution.Pretty                         (flatStyle, pretty)
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Version                        (VersionRange, simplifyVersionRange)

import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackagePath

import qualified Text.PrettyPrint as Disp
import Distribution.Solver.Types.ComponentDeps (Component(..))
import Distribution.Types.Dependency


-- | Determines to what packages and in what contexts a
-- constraint applies.
data ConstraintScope
     -- | A scope that applies when the given package is used as a build target.
     -- In other words, the scope applies iff a goal has a top-level qualifier
     -- and its namespace matches the given package name. A namespace is
     -- considered to match a package name when it is either the default
     -- namespace (for --no-independent-goals) or it is an independent namespace
     -- with the given package name (for --independent-goals).

   = ScopeTarget PackageName
     -- | The package with the specified name and qualifier.
   | ScopeQualified Namespace Qualifier PackageName

    -- Apply a constraint to a private-build-depends scope
    -- It is not sufficient to have ScopeQualified because we don't have enough
    -- information in the constraint syntax to fill in the `Component` field of
    -- `QualAlias`
   | ScopePrivate PackageName PrivateAlias PackageName
     -- | The package with the specified name when it has a
     -- setup qualifier.
   | ScopeAnySetupQualifier PackageName
     -- | The package with the specified name regardless of
     -- qualifier.
   | ScopeAnyQualifier PackageName
  deriving (Eq, Show)

-- | Constructor for a common use case: the constraint applies to
-- the package with the specified name when that package is a
-- top-level dependency in the default namespace.
scopeToplevel :: PackageName -> ConstraintScope
scopeToplevel = ScopeQualified DefaultNamespace QualToplevel

-- | Returns the package name associated with a constraint scope.
scopeToPackageName :: ConstraintScope -> PackageName
scopeToPackageName (ScopeTarget pn) = pn
scopeToPackageName (ScopeQualified _ _ pn) = pn
scopeToPackageName (ScopeAnySetupQualifier pn) = pn
scopeToPackageName (ScopeAnyQualifier pn) = pn
scopeToPackageName (ScopePrivate _ _ pn) = pn

-- | Whether a ConstraintScope matches a qualified package name, the crucial
-- function which determines the rules about when constraints apply.
constraintScopeMatches :: ConstraintScope -> QPN -> Bool
constraintScopeMatches (ScopeTarget pn) (Q (PackagePath ns q) pn') =
  let namespaceMatches DefaultNamespace = True
      namespaceMatches (Independent namespacePn) = pn == namespacePn
      namespaceMatches (IndependentComponent {}) = False
      namespaceMatches (IndependentBuildTool {}) = False
  in namespaceMatches ns && q == QualToplevel && pn == pn'
constraintScopeMatches (ScopePrivate spn alias c_pn) (Q (PackagePath _qual_ns q) c_pn') =
  let qualMatches (QualAlias qual_pn qual_alias) = spn == qual_pn && alias == qual_alias
      qualMatches _ = False
    -- TODO: Check whether any ns should subsume qual_ns (if private constraint scopes grow namespaces...)
  in qualMatches q && c_pn == c_pn'
constraintScopeMatches (ScopeQualified ns cq cpn) (Q (PackagePath qual_ns q) cpn') =
  ns == qual_ns && cq == q && cpn == cpn'

constraintScopeMatches (ScopeAnySetupQualifier pn) (Q pp pn') =
  let setup (PackagePath (IndependentComponent _ ComponentSetup) _) = True
      setup _                             = False
  in setup pp && pn == pn'
constraintScopeMatches (ScopeAnyQualifier pn) (Q _ pn') = pn == pn'

-- | Pretty-prints a constraint scope.
dispConstraintScope :: ConstraintScope -> Disp.Doc
dispConstraintScope (ScopeTarget pn) = pretty pn <<>> Disp.text "." <<>> pretty pn
dispConstraintScope (ScopeQualified ns _q pn) = dispNamespace ns <<>> pretty pn
dispConstraintScope (ScopePrivate pn alias p) = Disp.text "private." <<>> pretty pn <<>> Disp.text "." <<>> pretty @PrivateAlias alias <<>> Disp.text ":" <<>> pretty p
dispConstraintScope (ScopeAnySetupQualifier pn) = Disp.text "setup." <<>> pretty pn
dispConstraintScope (ScopeAnyQualifier pn) = Disp.text "any." <<>> pretty pn

-- | A package property is a logical predicate on packages.
data PackageProperty
   = PackagePropertyVersion   VersionRange
   | PackagePropertyInstalled
   | PackagePropertySource
   | PackagePropertyFlags     FlagAssignment
   | PackagePropertyStanzas   [OptionalStanza]
  deriving (Eq, Show, Generic)

instance Binary PackageProperty
instance Structured PackageProperty

-- | Pretty-prints a package property.
dispPackageProperty :: PackageProperty -> Disp.Doc
dispPackageProperty (PackagePropertyVersion verrange) = pretty verrange
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
packageConstraintToDependency :: PackageConstraint -> Maybe PackageVersionConstraint
packageConstraintToDependency (PackageConstraint scope prop) = toDep prop
  where
    toDep (PackagePropertyVersion vr) = Just $ PackageVersionConstraint (scopeToPackageName scope) vr
    toDep (PackagePropertyInstalled)  = Nothing
    toDep (PackagePropertySource)     = Nothing
    toDep (PackagePropertyFlags _)    = Nothing
    toDep (PackagePropertyStanzas _)  = Nothing
