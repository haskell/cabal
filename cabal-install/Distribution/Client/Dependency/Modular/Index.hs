module Distribution.Client.Dependency.Modular.Index where

import Data.List as L
import Data.Map as M
import Prelude hiding (pi)

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree

-- | An index contains information about package instances. This is a nested
-- dictionary. Package names are mapped to instances, which in turn is mapped
-- to info.
type Index = Map PN (Map I PInfo)

-- | Info associated with a package instance.
-- Currently, dependencies, flags, encapsulations and failure reasons.
-- Packages that have a failure reason recorded for them are disabled
-- globally, for reasons external to the solver. We currently use this
-- for shadowing which essentially is a GHC limitation, and for
-- installed packages that are broken.
data PInfo = PInfo (FlaggedDeps PN) FlagInfo Encaps (Maybe FailReason)
  deriving (Show)

-- | Encapsulations. A list of package names.
type Encaps = [PN]

mkIndex :: [(PN, I, PInfo)] -> Index
mkIndex xs = M.map M.fromList (groupMap (L.map (\ (pn, i, pi) -> (pn, (i, pi))) xs))

groupMap :: Ord a => [(a, b)] -> Map a [b]
groupMap xs = M.fromListWith (flip (++)) (L.map (\ (x, y) -> (x, [y])) xs)
