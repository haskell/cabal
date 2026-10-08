{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Distribution.Solver.Types.DependencyResolver
    ( ResolverParams (..)
    , DependencyResolver (..)
    ) where

import Distribution.Solver.Compat.Prelude
    ( Binary, Generic, Maybe, String, Set, Map
    )
import Prelude ()
import Data.Kind

import Distribution.Solver.Types.LabeledPackageConstraint
    ( LabeledPackageConstraint )
import Distribution.Solver.Types.PkgConfigDb ( PkgConfigDb )
import Distribution.Solver.Types.PackagePreferences
    ( PackagePreferences )
import Distribution.Solver.Types.PackageIndex ( PackageIndex )
import Distribution.Solver.Types.Progress
    ( Progress )
import Distribution.Solver.Types.ResolverPackage
    ( ResolverPackage )
import Distribution.Solver.Types.SourcePackage ( SourcePackage )
import Distribution.Solver.Types.SummarizedMessage
    ( SummarizedMessage(..) )
import Distribution.Simple.PackageIndex ( InstalledPackageIndex )
import Distribution.Package ( PackageName )
import Distribution.Compiler ( CompilerInfo )
import Distribution.System ( Platform )

data ResolverParams loc =
  ResolverParams
    { rpPlatform :: Platform
    , rpCompilerInfo :: CompilerInfo
    , rpInstalledPackages :: InstalledPackageIndex
    , rpSourcePackages :: PackageIndex (SourcePackage loc)
    , rpPkgConfigDb :: Maybe PkgConfigDb
    , rpPkgPrefs :: PackagesPreferenceSource
    , rpPkgConstraints :: [LabeledPackageConstraint]
    , rpPkgNames :: Set PackageName
    }
    deriving (Generic)

deriving instance Binary loc => Binary (ResolverParams loc)

-- | A dependency resolver consists of a function that works out an
-- installation plan given the set of installed and available packages and a
-- set of deps to solve for, together with a pair of resolver-specific input
-- and output types, and a pair of conversion that translate between these
-- resolver-specific types and the resolver-independent 'ResolverParams' type.
--
-- The reason for this interface is because there are dozens of approaches to
-- solving the package dependency problem and we want to make it easy to swap
-- in alternatives.
--
-- Both the input and the output type must be binary-serializable, because we
-- need to use them as keys for re-run checks.
class {- (Binary (ResolverInputs a), Binary (ResolverOutput a))
        => -} DependencyResolver a where
  type ResolverInputs a :: Type
  type ResolverOutput a :: Type
  runResolver :: a -> ResolverInputs a -> Progress SummarizedMessage String (ResolverOutput a)
  convInputs :: a -> ResolverParams loc -> ResolverInputs a
  convOutput :: a -> ResolverParams loc -> ResolverOutput a -> [ResolverPackage loc]

