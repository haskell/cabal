{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.ConfiguredPackage
    ( ConfiguredPackage(..)
    , fakeUnitId
    ) where

import Distribution.Compat.Binary (Binary(..))
import Distribution.Package (PackageId, UnitId, Package(..), HasUnitId(..), mkUnitId)
import Distribution.Text (display)
import Distribution.PackageDescription (FlagAssignment)
import Distribution.Solver.ComponentDeps (ComponentDeps)
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.SourcePackage
import Distribution.Solver.Types.ConfiguredId
import GHC.Generics (Generic)

-- | A 'ConfiguredPackage' is a not-yet-installed package along with the
-- total configuration information. The configuration information is total in
-- the sense that it provides all the configuration information and so the
-- final configure process will be independent of the environment.
--
data ConfiguredPackage loc = ConfiguredPackage
       (SourcePackage loc)     -- package info, including repo
       FlagAssignment          -- complete flag assignment for the package
       [OptionalStanza]        -- list of enabled optional stanzas for the package
       (ComponentDeps [ConfiguredId])
                               -- set of exact dependencies (installed or source).
                               -- These must be consistent with the 'buildDepends'
                               -- in the 'PackageDescription' that you'd get by
                               -- applying the flag assignment and optional stanzas.
  deriving (Eq, Show, Generic)

instance Binary loc => Binary (ConfiguredPackage loc)

instance Package (ConfiguredPackage loc) where
  packageId (ConfiguredPackage pkg _ _ _) = packageId pkg

instance HasUnitId (ConfiguredPackage loc) where
  installedUnitId = fakeUnitId . packageId

-- | In order to reuse the implementation of PackageIndex which relies on
-- 'UnitId', we need to be able to synthesize these IDs prior
-- to installation.  Eventually, we'll move to a representation of
-- 'UnitId' which can be properly computed before compilation
-- (of course, it's a bit of a misnomer since the packages are not actually
-- installed yet.)  In any case, we'll synthesize temporary installed package
-- IDs to use as keys during install planning.  These should never be written
-- out!  Additionally, they need to be guaranteed unique within the install
-- plan.
fakeUnitId :: PackageId -> UnitId
fakeUnitId = mkUnitId . (".fake."++) . display
