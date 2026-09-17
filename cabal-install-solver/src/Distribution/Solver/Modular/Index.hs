module Distribution.Solver.Modular.Index
    ( Index
    , StageIndex
    , PInfo(..)
    , ComponentInfo(..)
    , IsVisible(..)
    , IsBuildable(..)
    , defaultQualifyOptions
    , mkIndex
    ) where

import Prelude hiding (pi)

import Data.Map (Map)
import qualified Data.List as L
import qualified Data.Map as M

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Types.Stage (Stage (..))

-- | The packages available for a single build stage. This is a nested
-- dictionary. Package names are mapped to instances, which in turn is mapped
-- to info.
type StageIndex = Map PN (Map I PInfo)

-- | An index contains information about package instances, partitioned by build
-- 'Stage'. Each stage is solved against its own set of available packages,
-- because a different stage means a different compiler and therefore an
-- entirely different set of pre-installed packages. The goal being solved
-- always carries its stage (in its 'Distribution.Solver.Types.PackagePath.PackagePath'),
-- so lookups select the stage's 'StageIndex' first.
type Index = Map Stage StageIndex

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

mkIndex :: [(PN, I, PInfo)] -> StageIndex
mkIndex xs = M.map M.fromList (groupMap (L.map (\ (pn, i, pi) -> (pn, (i, pi))) xs))

groupMap :: Ord a => [(a, b)] -> Map a [b]
groupMap xs = M.fromListWith (flip (++)) (L.map (\ (x, y) -> (x, [y])) xs)

defaultQualifyOptions :: Index -> QualifyOptions
defaultQualifyOptions idx = QO {
      qoBaseShim         = or [ dep == base
                              | -- Look in the host stage's packages ..
                                Just hostIdx <- [M.lookup Host idx]
                                -- .. find all versions of base ..
                              , Just is <- [M.lookup base hostIdx]
                                -- .. which are installed ..
                              , (I _ver (Inst _), PInfo deps _comps _flagNfo _fr) <- M.toList is
                                -- .. and flatten all their dependencies ..
                              , (LDep _ (Dep (PkgComponent dep _) _ci), _comp) <- flattenFlaggedDeps deps
                              ]
    , qoSetupIndependent = True
      -- The index's stage-key set is the single source of truth for whether
      -- we are cross-compiling: 'convPIs' only populates a 'Build' stage when
      -- the build toolchain differs from the host one.
    , qoCross = M.member Build idx
    }
  where
    base = mkPackageName "base"
