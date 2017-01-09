{-# LANGUAGE DeriveGeneric #-}

-- | Per-package constraints. Package constraints must be respected by the
-- solver. Multiple constraints for each package can be given, though obviously
-- it is possible to construct conflicting constraints (eg impossible version
-- range or inconsistent flag assignment).
--
module Distribution.Solver.Types.PackageConstraint (
    PackageProperty(..),
    dispPackageProperty,
    PackageConstraint(..),
    dispPackageConstraint,
    showPackageConstraint,
  ) where

import Distribution.Version (VersionRange, simplifyVersionRange)
import Distribution.PackageDescription (FlagAssignment, dispFlagAssignment)
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackagePath (QPN, dispQPN)

import GHC.Generics (Generic)
import Distribution.Compat.Binary (Binary)

import Distribution.Text (disp, flatStyle)
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<+>))

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
dispPackageProperty PackagePropertyInstalled = Disp.text "installed"
dispPackageProperty PackagePropertySource = Disp.text "source"
dispPackageProperty (PackagePropertyFlags flags) = dispFlagAssignment flags
dispPackageProperty (PackagePropertyStanzas stanzas) =
  Disp.hsep $ map (Disp.text . showStanza) stanzas

-- | A package constraint consists of a package plus a property
-- that must hold for that package.
data PackageConstraint = PackageConstraint QPN PackageProperty
  deriving (Eq, Show, Generic)

instance Binary PackageConstraint

-- | Pretty-prints a package constraint.
dispPackageConstraint :: PackageConstraint -> Disp.Doc
dispPackageConstraint (PackageConstraint qpn prop) =
  dispQPN qpn <+> dispPackageProperty prop

-- | Alternative textual representation of a package constraint
-- for debugging purposes (slightly more verbose than that
-- produced by 'dispPackageConstraint').
--
showPackageConstraint :: PackageConstraint -> String
showPackageConstraint pc@(PackageConstraint qpn prop) =
  Disp.renderStyle flatStyle . postprocess $ dispPackageConstraint pc2
  where
    pc2 = case prop of
      PackagePropertyVersion vr ->
        PackageConstraint qpn $ PackagePropertyVersion (simplifyVersionRange vr)
      _ -> pc
    postprocess = case prop of
      PackagePropertyFlags _ -> (Disp.text "flags" <+>)
      PackagePropertyStanzas _ -> (Disp.text "stanzas" <+>)
      _ -> id
