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

import Distribution.Package                        (PackageName, UnitId)
import Distribution.PackageDescription             (FlagAssignment, dispFlagAssignment)
import Distribution.Pretty                         (flatStyle, Pretty(pretty))
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Version                        (VersionRange, simplifyVersionRange)

import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.Stage             (Stage (..))

import qualified Text.PrettyPrint as Disp


-- | Determines to what packages and in what contexts a constraint applies.
--
-- A scope pairs an optional build 'Stage' with a 'ConstraintQualifier'. When
-- the stage is 'Nothing' the constraint applies at every stage; when it is
-- @'Just' s@ it applies only to goals solved for stage @s@. (Top-level goals
-- are always solved at the 'Host' stage, so 'scopeToplevel' pins the stage to
-- @'Just' 'Host'@.)
data ConstraintScope =
    ConstraintScope (Maybe Stage) ConstraintQualifier
  deriving (Eq, Show)

-- | The qualifier part of a 'ConstraintScope': which goals a constraint
-- applies to, independently of the build stage.
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
-- top-level dependency in the default namespace. Top-level goals are always
-- solved at the 'Host' stage, so the scope is pinned there.
scopeToplevel :: PackageName -> ConstraintScope
scopeToplevel = ConstraintScope (Just Host) . ScopeQualified QualToplevel

-- | Returns the package name associated with a constraint scope.
scopeToPackageName :: ConstraintScope -> PackageName
scopeToPackageName (ConstraintScope _ (ScopeTarget pn)) = pn
scopeToPackageName (ConstraintScope _ (ScopeQualified _ pn)) = pn
scopeToPackageName (ConstraintScope _ (ScopeAnySetupQualifier pn)) = pn
scopeToPackageName (ConstraintScope _ (ScopeAnyQualifier pn)) = pn

constraintScopeMatches :: ConstraintScope -> QPN -> Bool
constraintScopeMatches (ConstraintScope mstage qualifier) (Q (PackagePath stage ns q) pn') =
    maybe True (== stage) mstage && constraintQualifierMatches qualifier ns q pn'

-- | Whether the qualifier part of a constraint scope matches a goal, given the
-- goal's namespace, qualifier and package name.
constraintQualifierMatches :: ConstraintQualifier -> Namespace -> Qualifier -> PackageName -> Bool
constraintQualifierMatches (ScopeTarget pn) ns q pn' =
    namespaceMatches ns && q == QualToplevel && pn == pn'
  where
    namespaceMatches DefaultNamespace = True
    namespaceMatches (Independent namespacePn) = pn == namespacePn
constraintQualifierMatches (ScopeQualified q pn) _ns q' pn' =
    q == q' && pn == pn'
constraintQualifierMatches (ScopeAnySetupQualifier pn) _ns q pn' =
    setup q && pn == pn'
  where
    setup (QualSetup _) = True
    setup _             = False
constraintQualifierMatches (ScopeAnyQualifier pn) _ns _q pn' = pn == pn'

-- | The stage is not rendered: there is no user syntax for it (yet), and the
-- only staged scopes are the 'Just' 'Host' ones made by 'scopeToplevel'.
instance Pretty ConstraintScope where
  pretty (ConstraintScope _mstage qualifier) = pretty qualifier

instance Pretty ConstraintQualifier where
  pretty (ScopeTarget pn) = pretty pn <<>> Disp.text "." <<>> pretty pn
  pretty (ScopeQualified q pn) = dispQualifier q <<>> pretty pn
  pretty (ScopeAnySetupQualifier pn) = Disp.text "setup." <<>> pretty pn
  pretty (ScopeAnyQualifier pn) = Disp.text "any." <<>> pretty pn

-- | A package property is a logical predicate on packages.
data PackageProperty
   = PackagePropertyVersion   VersionRange
   | PackagePropertyInstalled
   | PackagePropertyInstalledSpecificUnitId UnitId
   | PackagePropertySource
   | PackagePropertyFlags     FlagAssignment
   | PackagePropertyStanzas   [OptionalStanza]
  deriving (Eq, Show, Generic)

instance Binary PackageProperty
instance NFData PackageProperty
instance Structured PackageProperty

instance Pretty PackageProperty where
  pretty (PackagePropertyVersion verrange) = pretty verrange
  pretty PackagePropertyInstalled          = Disp.text "installed"
  pretty (PackagePropertyInstalledSpecificUnitId unitId) = Disp.text "installed(" <> pretty unitId <> Disp.text ")"
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
    toDep PackagePropertyInstalled  = Nothing
    toDep (PackagePropertyInstalledSpecificUnitId {}) = Nothing
    toDep PackagePropertySource       = Nothing
    toDep (PackagePropertyFlags _)    = Nothing
    toDep (PackagePropertyStanzas _)  = Nothing
