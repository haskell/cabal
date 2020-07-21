module Distribution.Solver.Modular.Version
    ( Ver
    , VR
    , anyVR
    , checkVR
    , eqVR
    , showVer
    , showVR
    , simplifyVR
    , (.&&.)
    , (.||.)
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import qualified Distribution.Version as CV -- from Cabal
import Distribution.Pretty (prettyShow)

-- | Preliminary type for versions.
type Ver = CV.Version

-- | String representation of a version.
showVer :: Ver -> String
showVer = prettyShow

-- | Version range. Consists of a lower and upper bound.
type VR = CV.VersionRange

-- | String representation of a version range.
showVR :: VR -> String
showVR = prettyShow

-- | Unconstrained version range.
anyVR :: VR
anyVR = CV.anyVersion

-- | Version range fixing a single version.
eqVR :: Ver -> VR
eqVR = CV.thisVersion

-- | Intersect two version ranges.
(.&&.) :: VR -> VR -> VR
v1 .&&. v2 = simplifyVR $ CV.intersectVersionRanges v1 v2

-- | Union of two version ranges.
(.||.) :: VR -> VR -> VR
v1 .||. v2 = simplifyVR $ CV.unionVersionRanges v1 v2

-- | Simplify a version range.
simplifyVR :: VR -> VR
simplifyVR = CV.simplifyVersionRange

-- | Checking a version against a version range.
checkVR :: VR -> Ver -> Bool
checkVR = flip CV.withinRange
