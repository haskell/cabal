{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.PackageConstraint (
    PackageProperty(..),
    PackageConstraint(..),
--    showPackageConstraint,
  ) where

import Distribution.Compat.Binary (Binary(..))
import Distribution.PackageDescription (FlagAssignment, FlagName(..))
import Distribution.Package (PackageName)
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackagePath (Qualified)
import Distribution.Text (Text(..), display)
import Distribution.Version (VersionRange, simplifyVersionRange)
import GHC.Generics (Generic)


data PackageProperty
   = PackagePropertyVersion   VersionRange
   | PackagePropertyInstalled
   | PackagePropertySource
   | PackagePropertyFlags     FlagAssignment
   | PackagePropertyStanzas   [OptionalStanza]
  deriving (Eq, Show, Generic)

instance Binary PackageProperty

data PackageConstraint
  = PackageConstraint (Qualified PackageName) PackageProperty
  deriving (Eq, Show, Generic)

instance Binary PackageConstraint

dispPackageProperty :: PackageProperty -> Disp.Doc
  disp (PackagePropertyVersion   verrange) = disp verrange
  disp PackagePropertyInstalled            = Disp.text "installed"
  disp PackagePropertySource               = Disp.text "source"
  disp (PackagePropertyFlags     flags)    = dispFlagAssignment flags
  disp (PackagePropertyStanzas   stanzas)  = dispStanzas stanzas
    where
      dispStanzas = Disp.hsep . map dispStanza
      dispStanza TestStanzas  = Disp.text "test"
      dispStanza BenchStanzas = Disp.text "bench"

dispPackageConstraint :: PackageConstraint -> Disp.Doc
dispPackageConstraint (PackageConstraint (Q path name) pp) =


-- | Provide a textual representation of a package constraint
-- for debugging purposes.

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
