module Distribution.Solver.Modular.Configured
    ( CP(..)
    ) where

import Distribution.PackageDescription (FlagAssignment) -- from Cabal
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.ComponentDeps (ComponentDeps)
import Distribution.Solver.Modular.Package

-- | A configured package is a package instance together with
-- a flag assignment and complete dependencies.
data CP qpn = CP (PI qpn) FlagAssignment [OptionalStanza] (ComponentDeps [PI qpn])
