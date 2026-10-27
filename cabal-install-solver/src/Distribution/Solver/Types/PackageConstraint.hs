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
import Distribution.Solver.Types.GoalScope
import Distribution.Solver.Types.Machine             (Machine (..), showMachine)

import qualified Text.PrettyPrint as Disp


-- | Determines to what packages and in what contexts a constraint applies.
--
-- A scope pairs an optional 'Machine' with a 'ConstraintQualifier'. When
-- the machine is 'Nothing' the constraint applies on every machine; when it is
-- @'Just' s@ it applies only to goals solved for machine @s@. (Top-level goals
-- are always solved at the 'Host' machine, so 'scopeToplevel' pins the machine to
-- @'Just' 'Host'@.)
data ConstraintScope =
    ConstraintScope (Maybe Machine) ConstraintQualifier
  deriving (Eq, Show)

-- | The qualifier part of a 'ConstraintScope': which goals a constraint
-- applies to, independently of the build machine.
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
     -- | The package with the specified name when it has an
     -- executable (build-tool) qualifier.
   | ScopeAnyExeQualifier PackageName
     -- | The package with the specified name regardless of
     -- qualifier.
   | ScopeAnyQualifier PackageName
  deriving (Eq, Show)

-- | Constructor for a common use case: the constraint applies to
-- the package with the specified name when that package is a
-- top-level dependency in the default namespace. Top-level goals are always
-- solved at the 'Host' machine, so the scope is pinned there.
scopeToplevel :: PackageName -> ConstraintScope
scopeToplevel = ConstraintScope (Just Host) . ScopeQualified QualToplevel

-- | Returns the package name associated with a constraint scope.
scopeToPackageName :: ConstraintScope -> PackageName
scopeToPackageName (ConstraintScope _ (ScopeTarget pn)) = pn
scopeToPackageName (ConstraintScope _ (ScopeQualified _ pn)) = pn
scopeToPackageName (ConstraintScope _ (ScopeAnySetupQualifier pn)) = pn
scopeToPackageName (ConstraintScope _ (ScopeAnyExeQualifier pn)) = pn
scopeToPackageName (ConstraintScope _ (ScopeAnyQualifier pn)) = pn

constraintScopeMatches :: ConstraintScope -> QPN -> Bool
constraintScopeMatches (ConstraintScope mmachine qualifier) (Q (GoalScope machine ns q) pn') =
    maybe True (== machine) mmachine && constraintQualifierMatches qualifier ns q pn'

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
constraintQualifierMatches (ScopeAnyExeQualifier pn) _ns q pn' =
    exe q && pn == pn'
  where
    exe (QualExe _ _) = True
    exe _             = False
constraintQualifierMatches (ScopeAnyQualifier pn) _ns _q pn' = pn == pn'

instance Pretty ConstraintScope where
  pretty (ConstraintScope mmachine qualifier) = dispMachinePrefix mmachine <<>> pretty qualifier
    where
      -- The 'Host' machine is the invisible default, so only 'Build' is rendered.
      dispMachinePrefix (Just Build) = Disp.text (showMachine Build) <<>> Disp.text ":"
      dispMachinePrefix _            = Disp.empty

instance Pretty ConstraintQualifier where
  pretty (ScopeTarget pn) = pretty pn <<>> Disp.text "." <<>> pretty pn
  pretty (ScopeQualified q pn) = dispQualifier q <<>> pretty pn
  pretty (ScopeAnySetupQualifier pn) = Disp.text "setup." <<>> pretty pn
  pretty (ScopeAnyExeQualifier pn) = Disp.text "exe." <<>> pretty pn
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
