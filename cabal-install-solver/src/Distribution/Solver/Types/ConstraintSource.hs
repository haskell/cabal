{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.ConstraintSource
    ( ConstraintSource(..)
    , showConstraintSource
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

-- | Source of a 'PackageConstraint'.
data ConstraintSource =

  -- | Main config file, which is ~/.cabal/config by default.
  ConstraintSourceMainConfig FilePath

  -- | Local cabal.project file
  | ConstraintSourceProjectConfig FilePath

  -- | User config file, which is ./cabal.config by default.
  | ConstraintSourceUserConfig FilePath

  -- | Flag specified on the command line.
  | ConstraintSourceCommandlineFlag

  -- | Target specified by the user, e.g., @cabal install package-0.1.0.0@
  -- implies @package==0.1.0.0@.
  | ConstraintSourceUserTarget

  -- | Internal requirement to use installed versions of packages like ghc-prim.
  | ConstraintSourceNonReinstallablePackage

  -- | Internal constraint used by @cabal freeze@.
  | ConstraintSourceFreeze

  -- | Constraint specified by a config file, a command line flag, or a user
  -- target, when a more specific source is not known.
  | ConstraintSourceConfigFlagOrTarget

  -- | Constraint introduced by --enable-multi-repl, which requires features
  -- from Cabal >= 3.11
  | ConstraintSourceMultiRepl

  -- | The source of the constraint is not specified.
  | ConstraintSourceUnknown

  -- | An internal constraint due to compatibility issues with the Setup.hs
  -- command line interface requires a minimum lower bound on Cabal
  | ConstraintSetupCabalMinVersion

  -- | An internal constraint due to compatibility issues with the Setup.hs
  -- command line interface requires a maximum upper bound on Cabal
  | ConstraintSetupCabalMaxVersion
  deriving (Eq, Show, Generic)

instance Binary ConstraintSource
instance Structured ConstraintSource

-- | Description of a 'ConstraintSource'.
showConstraintSource :: ConstraintSource -> String
showConstraintSource (ConstraintSourceMainConfig path) =
    "main config " ++ path
showConstraintSource (ConstraintSourceProjectConfig path) =
    "project config " ++ path
showConstraintSource (ConstraintSourceUserConfig path)= "user config " ++ path
showConstraintSource ConstraintSourceCommandlineFlag = "command line flag"
showConstraintSource ConstraintSourceUserTarget = "user target"
showConstraintSource ConstraintSourceNonReinstallablePackage =
    "non-reinstallable package"
showConstraintSource ConstraintSourceFreeze = "cabal freeze"
showConstraintSource ConstraintSourceConfigFlagOrTarget =
    "config file, command line flag, or user target"
showConstraintSource ConstraintSourceMultiRepl =
    "--enable-multi-repl"
showConstraintSource ConstraintSourceUnknown = "unknown source"
showConstraintSource ConstraintSetupCabalMinVersion =
    "minimum version of Cabal used by Setup.hs"
showConstraintSource ConstraintSetupCabalMaxVersion =
    "maximum version of Cabal used by Setup.hs"
