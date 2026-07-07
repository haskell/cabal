module Distribution.Solver.Types.DependencyResolver
    ( DependencyResolver
    ) where

import Distribution.Solver.Compat.Prelude ( Maybe, String, Set )
import Prelude ()

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
import Distribution.Solver.Types.Stage ( Staged )

-- | A dependency resolver is a function that works out an installation plan
-- given the set of installed and available packages and a set of deps to
-- solve for.
--
-- The reason for this interface is because there are dozens of approaches to
-- solving the package dependency problem and we want to make it easy to swap
-- in alternatives.
--
-- The platform, compiler and installed-package index are provided per build
-- 'Distribution.Solver.Types.Stage.Stage' (as 'Staged'), so the solver can
-- target a different toolchain for the build machine and the host machine when
-- cross-compiling. In an ordinary build both stages hold the same value. The
-- source package index is shared across stages.
--
type DependencyResolver loc = Staged Platform
                           -> Staged CompilerInfo
                           -> Staged InstalledPackageIndex
                           -> PackageIndex (SourcePackage loc)
                           -> Maybe PkgConfigDb
                           -> (PackageName -> PackagePreferences)
                           -> [LabeledPackageConstraint]
                           -> Set PackageName
                           -> Progress SummarizedMessage String [ResolverPackage loc]
