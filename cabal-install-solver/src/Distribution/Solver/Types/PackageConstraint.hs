{-# LANGUAGE DeriveGeneric #-}

-- | Per-package constraints. Package constraints must be respected by the
-- solver. Multiple constraints for each package can be given, though obviously
-- it is possible to construct conflicting constraints (eg impossible version
-- range or inconsistent flag assignment).
--
module Distribution.Solver.Types.PackageConstraint (
    ConstraintScope(..),
    ConstraintQualifier(..),
    scopeToplevel,
    scopeToPackageName,
    constraintScopeMatches,
    PackageProperty(..),
    PackageConstraint(..),
    showPackageConstraint,
    packageConstraintToDependency
  ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Package                        (PackageName)
import Distribution.PackageDescription             (FlagAssignment, dispFlagAssignment)
import Distribution.Pretty                         (flatStyle, Pretty(pretty))
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Version                        (VersionRange, simplifyVersionRange)

import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackagePath

import qualified Text.PrettyPrint as Disp
import Distribution.Solver.Types.Toolchain (Stage (..))


-- | Determines to what packages and in what contexts a
-- constraint applies.
data ConstraintScope =
    ConstraintScope
      -- | The stage at which the constraint applies, if any.
      -- If Nothing, the constraint applies to all stages.
    (Maybe Stage)
      -- | The qualifier that determines the scope of the constraint.
    ConstraintQualifier
  deriving (Eq, Show)

data ConstraintQualifier
     -- | A scope that applies when the given package is used as a build target.
     -- In other words, the scope applies iff a goal has a top-level qualifier
     -- and its namespace matches the given package name. A namespace is
     -- considered to match a package name when it is either the default
     -- namespace (for --no-independent-goals) or it is an independent namespace
     -- with the given package name (for --independent-goals).

     -- TODO: Try to generalize the ConstraintScopes once component-based
     -- solving is implemented, and remove this special case for targets.
   = ScopeTarget PackageName
     -- | The package with the specified name and qualifier.
   | ScopeQualified Qualifier PackageName
     -- | The package with the specified name when it has a
     -- setup qualifier.
   | ScopeAnySetupQualifier PackageName
     -- | The package with the specified name regardless of
     -- qualifier.
   | ScopeAnyQualifier PackageName
  deriving (Eq, Show)

-- | Constructor for a common use case: the constraint applies to
-- the package with the specified name when that package is a
-- top-level dependency in the host stage.
scopeToplevel :: PackageName -> ConstraintScope
scopeToplevel = ConstraintScope (Just Host) . ScopeQualified QualToplevel

-- | Returns the package name associated with a constraint scope.
scopeToPackageName :: ConstraintScope -> PackageName
scopeToPackageName (ConstraintScope _stage (ScopeTarget pn)) = pn
scopeToPackageName (ConstraintScope _stage (ScopeQualified _ pn)) = pn
scopeToPackageName (ConstraintScope _stage (ScopeAnySetupQualifier pn)) = pn
scopeToPackageName (ConstraintScope _stage (ScopeAnyQualifier pn)) = pn

constraintScopeMatches :: ConstraintScope -> QPN -> Bool
constraintScopeMatches (ConstraintScope mstage qualifier) (Q (PackagePath stage' q) pn') =
  maybe True (== stage') mstage && constraintQualifierMatches qualifier q pn'

constraintQualifierMatches :: ConstraintQualifier -> Qualifier -> PackageName -> Bool
constraintQualifierMatches (ScopeTarget pn) q pn' =
    q == QualToplevel && pn == pn'
constraintQualifierMatches (ScopeQualified q pn) q' pn' =
    q == q' && pn == pn'
constraintQualifierMatches (ScopeAnySetupQualifier pn) (QualSetup _) pn' =
  pn == pn'
constraintQualifierMatches (ScopeAnyQualifier pn) _ pn' =
    pn == pn'
constraintQualifierMatches _ _ _ = False

instance Pretty ConstraintScope where
  pretty (ConstraintScope mstage qualifier) =
    maybe mempty pretty mstage <+> pretty qualifier

instance Pretty ConstraintQualifier where
  pretty (ScopeTarget pn) = pretty pn <<>> Disp.text "." <<>> pretty pn
  pretty (ScopeQualified q pn) = dispQualifier q <<>> pretty pn
  pretty (ScopeAnySetupQualifier pn) = Disp.text "setup." <<>> pretty pn
  pretty (ScopeAnyQualifier pn) = Disp.text "any." <<>> pretty pn

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

instance Pretty PackageProperty where
  pretty (PackagePropertyVersion verrange) = pretty verrange
  pretty PackagePropertyInstalled          = Disp.text "installed"
  pretty PackagePropertySource             = Disp.text "source"
  pretty (PackagePropertyFlags flags)      = dispFlagAssignment flags
  pretty (PackagePropertyStanzas stanzas)  =
    Disp.hsep $ map (Disp.text . showStanza) stanzas

-- | A package constraint consists of a scope plus a property
-- that must hold for all packages within that scope.
data PackageConstraint = PackageConstraint ConstraintScope PackageProperty
  deriving (Eq, Show)

instance Pretty PackageConstraint where
  pretty (PackageConstraint scope prop) =
    pretty scope <+> pretty prop

-- | Alternative textual representation of a package constraint
-- for debugging purposes (slightly more verbose than that
-- produced by 'dispPackageConstraint').
--
showPackageConstraint :: PackageConstraint -> String
showPackageConstraint pc@(PackageConstraint scope prop) =
  Disp.renderStyle flatStyle . postprocess $ pretty pc2
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
