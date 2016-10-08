{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.PackageConstraint (
    PackageConstraint(..),
    showPackageConstraint,
    PackagesSubsetConstraint(..),
    SubsetName,
  ) where

import Distribution.Compat.Binary (Binary(..))
import Distribution.PackageDescription (FlagAssignment, FlagName(..))
import Distribution.Package (PackageName)
import Distribution.Solver.Types.OptionalStanza
import Distribution.Text (display)
import Distribution.Version (VersionRange, simplifyVersionRange)
import GHC.Generics (Generic)
import Data.Map (Map)

-- | Per-package constraints. Package constraints must be respected by the
-- solver. Multiple constraints for each package can be given, though obviously
-- it is possible to construct conflicting constraints (eg impossible version
-- range or inconsistent flag assignment).
--
data PackageConstraint
   = PackageConstraintVersion   PackageName VersionRange
   | PackageConstraintInstalled PackageName
   | PackageConstraintSource    PackageName
   | PackageConstraintFlags     PackageName FlagAssignment
   | PackageConstraintStanzas   PackageName [OptionalStanza]
  deriving (Eq, Show, Generic)

instance Binary PackageConstraint

{-
data   
  = Ordinary                     PackageConstraint
  | Implication PackageId {-  -} PackageConstraint
-}

-- | A set of constraints to select only from within a subset of packages.
-- Packages in the subset are in the map, (which gives the set of versions
-- for that package) while packages not in the map are excluded entirely.
--
-- The subset has a name, to be used for solver error message reporting.
--
-- This is equivalent to a list of 'PackageConstraint' but with a rather more
-- compact representation. (A list would need to cover every single package
-- simply to be able to exclude those not in the subset)
--
data PackagesSubsetConstraint
   = NoPackagesSubsetSelected
   | PackagesSubsetSelected !(Map PackageName VersionRange)
                            !SubsetName
  deriving (Eq, Show, Generic)

type SubsetName = String

-- | Provide a textual representation of a package constraint
-- for debugging purposes.
--
showPackageConstraint :: PackageConstraint -> String
showPackageConstraint (PackageConstraintVersion pn vr) =
  display pn ++ " " ++ display (simplifyVersionRange vr)
showPackageConstraint (PackageConstraintInstalled pn) =
  display pn ++ " installed"
showPackageConstraint (PackageConstraintSource pn) =
  display pn ++ " source"
showPackageConstraint (PackageConstraintFlags pn fs) =
  "flags " ++ display pn ++ " " ++ unwords (map (uncurry showFlag) fs)
  where
    showFlag (FlagName f) True  = "+" ++ f
    showFlag (FlagName f) False = "-" ++ f
showPackageConstraint (PackageConstraintStanzas pn ss) =
  "stanzas " ++ display pn ++ " " ++ unwords (map showStanza ss)
  where
    showStanza TestStanzas  = "test"
    showStanza BenchStanzas = "bench"
