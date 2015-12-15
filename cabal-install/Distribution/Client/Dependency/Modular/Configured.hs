module Distribution.Client.Dependency.Modular.Configured
    ( CP(..)
    ) where

import Distribution.PackageDescription (FlagAssignment) -- from Cabal
import Distribution.Client.Types (OptionalStanza)
import Distribution.Client.ComponentDeps (ComponentDeps)

import Distribution.Client.Dependency.Modular.Package

-- | A configured package is a package instance together with
-- a flag assignment and complete dependencies.
data CP qpn = CP (PI qpn) FlagAssignment [OptionalStanza] (ComponentDeps [PI qpn])
