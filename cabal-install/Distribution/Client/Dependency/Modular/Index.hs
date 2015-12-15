module Distribution.Client.Dependency.Modular.Index
    ( Index
    , PInfo(..)
    , defaultQualifyOptions
    , mkIndex
    ) where

import Data.List as L
import Data.Map as M
import Prelude hiding (pi)

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree

import Distribution.Client.ComponentDeps (Component)

-- | An index contains information about package instances. This is a nested
-- dictionary. Package names are mapped to instances, which in turn is mapped
-- to info.
type Index = Map PN (Map I PInfo)

-- | Info associated with a package instance.
-- Currently, dependencies, flags and failure reasons.
-- Packages that have a failure reason recorded for them are disabled
-- globally, for reasons external to the solver. We currently use this
-- for shadowing which essentially is a GHC limitation, and for
-- installed packages that are broken.
data PInfo = PInfo (FlaggedDeps Component PN) FlagInfo (Maybe FailReason)
  deriving (Show)

mkIndex :: [(PN, I, PInfo)] -> Index
mkIndex xs = M.map M.fromList (groupMap (L.map (\ (pn, i, pi) -> (pn, (i, pi))) xs))

groupMap :: Ord a => [(a, b)] -> Map a [b]
groupMap xs = M.fromListWith (flip (++)) (L.map (\ (x, y) -> (x, [y])) xs)

defaultQualifyOptions :: Index -> QualifyOptions
defaultQualifyOptions idx = QO {
      qoBaseShim         = or [ dep == base
                              | -- Find all versions of base ..
                                Just is <- [M.lookup base idx]
                                -- .. which are installed ..
                              , (I _ver (Inst _), PInfo deps _flagNfo _fr) <- M.toList is
                                -- .. and flatten all their dependencies ..
                              , (Dep dep _ci, _comp) <- flattenFlaggedDeps deps
                              ]
    , qoSetupIndependent = True
    }
  where
    base = PackageName "base"
