module Distribution.Client.Dependency.Modular.Index where

import Data.List as L
import Data.Map as M
import Prelude hiding (pi)

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package

-- | An index contains information about package instances. This is a nested
-- dictionary. Package names are mapped to instances, which in turn is mapped
-- to info.
type Index = Map PN (Map I PInfo)

-- | Info associated with a package instance.
-- Currently, dependencies, flags and encapsulations.
data PInfo = PInfo (FlaggedDeps PN) FlagDefaults Encaps
  deriving (Show)

-- | Encapsulations. A list of package names.
type Encaps = [PN]

mkIndex :: [(PN, I, PInfo)] -> Index
mkIndex xs = M.map M.fromList (groupMap (L.map (\ (pn, i, pi) -> (pn, (i, pi))) xs))

groupMap :: Ord a => [(a, b)] -> Map a [b]
groupMap xs = M.fromListWith (++) (L.map (\ (x, y) -> (x, [y])) xs)
