module Distribution.Solver.Modular.Index
    ( Index
    , MachineIndex
    , PInfo(..)
    , ComponentInfo(..)
    , IsVisible(..)
    , IsBuildable(..)
    , defaultQualifyOptions
    , mkIndex
    ) where

import Prelude hiding (pi)

import Control.Monad (void)

import Data.Map (Map)
import qualified Data.List as L
import qualified Data.Map as M

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Types.Machine (PerMachine (..))

-- | The packages available for a single build machine. This is a nested
-- dictionary. Package names are mapped to instances, which in turn is mapped
-- to info.
type MachineIndex = Map PN (Map I PInfo)

-- | An index contains information about package instances, partitioned by build
-- 'Machine'. Each machine is solved against its own set of available packages,
-- because a different machine means a different compiler and therefore an
-- entirely different set of pre-installed packages. The goal being solved
-- always carries its machine (in its 'Distribution.Solver.Types.GoalScope.GoalScope'),
-- so lookups select the machine's 'MachineIndex' first with 'onMachine'.
--
-- The build machine is present exactly when there is a distinct build
-- toolchain ('isCross'); a non-cross build has only the host index.
type Index = PerMachine MachineIndex

-- | Info associated with a package instance.
-- Currently, dependencies, component names, flags and failure reasons.
-- The component map records whether any components are unbuildable in the
-- current environment (compiler, os, arch, and global flag constraints).
-- Packages that have a failure reason recorded for them are disabled
-- globally, for reasons external to the solver. We currently use this
-- for shadowing which essentially is a GHC limitation, and for
-- installed packages that are broken.
data PInfo = PInfo (FlaggedDeps PN)
                   (Map ExposedComponent ComponentInfo)
                   FlagInfo
                   (Maybe FailReason)

-- | Info associated with each library and executable in a package instance.
data ComponentInfo = ComponentInfo {
    compIsVisible   :: IsVisible
  , compIsBuildable :: IsBuildable
  }
  deriving Show

-- | Whether a component is visible in the current environment.
newtype IsVisible = IsVisible Bool
  deriving (Eq, Show)

-- | Whether a component is made unbuildable by a "buildable: False" field.
newtype IsBuildable = IsBuildable Bool
  deriving (Eq, Show)

mkIndex :: [(PN, I, PInfo)] -> MachineIndex
mkIndex xs = M.map M.fromList (groupMap (L.map (\ (pn, i, pi) -> (pn, (i, pi))) xs))

groupMap :: Ord a => [(a, b)] -> Map a [b]
groupMap xs = M.fromListWith (flip (++)) (L.map (\ (x, y) -> (x, [y])) xs)

defaultQualifyOptions :: Index -> QualifyOptions
defaultQualifyOptions idx = QO {
      qoBaseShim         = or [ dep == base
                              | -- Look in the host machine's packages ..
                                Just is <- [M.lookup base (onHost idx)]
                                -- .. which are installed ..
                              , (I _ver (Inst _), PInfo deps _comps _flagNfo _fr) <- M.toList is
                                -- .. and flatten all their dependencies ..
                              , (LDep _ (Dep (PkgComponent dep _) _ci), _comp) <- flattenFlaggedDeps deps
                              ]
    , qoSetupIndependent = True
      -- The index has the machine shape of the toolchains it was converted
      -- from (see 'convPIs'), so this is 'isCross' of those toolchains: a
      -- build machine exists only when there is a distinct build toolchain.
    , qoMachines = void idx
    }
  where
    base = mkPackageName "base"
