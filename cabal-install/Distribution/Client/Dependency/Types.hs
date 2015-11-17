{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Dependency.Types
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Common types for dependency resolution.
-----------------------------------------------------------------------------
module Distribution.Client.Dependency.Types (
    PreSolver(..),
    Solver(..),
    DependencyResolver,

    AllowNewer(..), isAllowNewer,
    PackageConstraint(..),
    showPackageConstraint,
    PackagePreferences(..),
    InstalledPreference(..),
    PackagesPreferenceDefault(..),

    Progress(..),
    foldProgress,

    LabeledPackageConstraint(..),
    ConstraintSource(..),
    unlabelPackageConstraint,
    showConstraintSource
  ) where

import Data.Char
         ( isAlpha, toLower )

import Distribution.Solver.Types
         ( DependencyResolver
         , PackageConstraint(..), PackagePreferences(..)
         , InstalledPreference(..), Progress(..)
         , LabeledPackageConstraint(..), ConstraintSource(..)
         , foldProgress
         , showConstraintSource
         , showPackageConstraint
         , unlabelPackageConstraint
         )

import qualified Distribution.Compat.ReadP as Parse
         ( pfail, munch1 )
import Distribution.Package
         ( PackageName )
import Distribution.Text
         ( Text(..) )

import Text.PrettyPrint
         ( text )

import Prelude hiding (fail)


-- | All the solvers that can be selected.
data PreSolver = AlwaysTopDown | AlwaysModular | Choose
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | All the solvers that can be used.
data Solver = TopDown | Modular
  deriving (Eq, Ord, Show, Bounded, Enum)

instance Text PreSolver where
  disp AlwaysTopDown = text "topdown"
  disp AlwaysModular = text "modular"
  disp Choose        = text "choose"
  parse = do
    name <- Parse.munch1 isAlpha
    case map toLower name of
      "topdown" -> return AlwaysTopDown
      "modular" -> return AlwaysModular
      "choose"  -> return Choose
      _         -> Parse.pfail

-- | Global policy for all packages to say if we prefer package versions that
-- are already installed locally or if we just prefer the latest available.
--
data PackagesPreferenceDefault =

     -- | Always prefer the latest version irrespective of any existing
     -- installed version.
     --
     -- * This is the standard policy for upgrade.
     --
     PreferAllLatest

     -- | Always prefer the installed versions over ones that would need to be
     -- installed. Secondarily, prefer latest versions (eg the latest installed
     -- version or if there are none then the latest source version).
   | PreferAllInstalled

     -- | Prefer the latest version for packages that are explicitly requested
     -- but prefers the installed version for any other packages.
     --
     -- * This is the standard policy for install.
     --
   | PreferLatestForSelected
  deriving Show

-- | Policy for relaxing upper bounds in dependencies. For example, given
-- 'build-depends: array >= 0.3 && < 0.5', are we allowed to relax the upper
-- bound and choose a version of 'array' that is greater or equal to 0.5? By
-- default the upper bounds are always strictly honored.
data AllowNewer =

  -- | Default: honor the upper bounds in all dependencies, never choose
  -- versions newer than allowed.
  AllowNewerNone

  -- | Ignore upper bounds in dependencies on the given packages.
  | AllowNewerSome [PackageName]

  -- | Ignore upper bounds in dependencies on all packages.
  | AllowNewerAll

-- | Convert 'AllowNewer' to a boolean.
isAllowNewer :: AllowNewer -> Bool
isAllowNewer AllowNewerNone     = False
isAllowNewer (AllowNewerSome _) = True
isAllowNewer AllowNewerAll      = True
