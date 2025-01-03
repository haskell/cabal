{-# LANGUAGE DeriveGeneric #-}

module Distribution.Solver.Types.ConstraintSource
  ( ConstraintSource (..)
  , showConstraintSource
  ) where

import Distribution.Pretty (Pretty (pretty), prettyShow)
import Distribution.Solver.Compat.Prelude
import Distribution.Solver.Types.ProjectConfigPath (ProjectConfigPath, docProjectConfigPath)
import Text.PrettyPrint (text)

-- | Source of a 'PackageConstraint'.
data ConstraintSource
  = -- | Main config file, which is ~/.cabal/config by default.
    ConstraintSourceMainConfig FilePath
  | -- | Local cabal.project file
    ConstraintSourceProjectConfig ProjectConfigPath
  | -- | User config file, which is ./cabal.config by default.
    ConstraintSourceUserConfig FilePath
  | -- | Flag specified on the command line.
    ConstraintSourceCommandlineFlag
  | -- | Target specified by the user, e.g., @cabal install package-0.1.0.0@
    -- implies @package==0.1.0.0@.
    ConstraintSourceUserTarget
  | -- | Internal requirement to use installed versions of packages like ghc-prim.
    ConstraintSourceNonReinstallablePackage
  | -- | Internal constraint used by @cabal freeze@.
    ConstraintSourceFreeze
  | -- | Constraint specified by a config file, a command line flag, or a user
    -- target, when a more specific source is not known.
    ConstraintSourceConfigFlagOrTarget
  | -- | Constraint introduced by --enable-multi-repl, which requires features
    -- from Cabal >= 3.11
    ConstraintSourceMultiRepl
  | -- | Constraint introduced by --enable-profiling-shared, which requires features
    -- from Cabal >= 3.13
    ConstraintSourceProfiledDynamic
  | -- | The source of the constraint is not specified.
    ConstraintSourceUnknown
  | -- | An internal constraint due to compatibility issues with the Setup.hs
    -- command line interface requires a minimum lower bound on Cabal
    ConstraintSetupCabalMinVersion
  | -- | An internal constraint due to compatibility issues with the Setup.hs
    -- command line interface requires a maximum upper bound on Cabal
    ConstraintSetupCabalMaxVersion
  deriving (Show, Eq, Generic)

instance Binary ConstraintSource
instance Structured ConstraintSource

-- | Description of a 'ConstraintSource'.
showConstraintSource :: ConstraintSource -> String
showConstraintSource = prettyShow

instance Pretty ConstraintSource where
  pretty constraintSource = case constraintSource of
    (ConstraintSourceMainConfig path) ->
      text "main config" <+> text path
    (ConstraintSourceProjectConfig path) ->
      text "project config" <+> docProjectConfigPath path
    (ConstraintSourceUserConfig path) -> text "user config " <+> text path
    ConstraintSourceCommandlineFlag -> text "command line flag"
    ConstraintSourceUserTarget -> text "user target"
    ConstraintSourceNonReinstallablePackage ->
      text "non-reinstallable package"
    ConstraintSourceFreeze -> text "cabal freeze"
    ConstraintSourceConfigFlagOrTarget ->
      text "config file, command line flag, or user target"
    ConstraintSourceMultiRepl ->
      text "--enable-multi-repl"
    ConstraintSourceProfiledDynamic ->
      text "--enable-profiling-shared"
    ConstraintSourceUnknown -> text "unknown source"
    ConstraintSetupCabalMinVersion ->
      text "minimum version of Cabal used by Setup.hs"
    ConstraintSetupCabalMaxVersion ->
      text "maximum version of Cabal used by Setup.hs"
