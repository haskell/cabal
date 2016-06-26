{-# LANGUAGE DeriveFunctor #-}
module Distribution.Solver.Modular.Package
  ( I(..)
  , Loc(..)
  , PackageId
  , PackageIdentifier(..)
  , PackageName(..)
  , PI(..)
  , PN
  , QPV
  , instI
  , makeIndependent
  , primaryPP
  , showI
  , showPI
  , unPN
  ) where

import Data.List as L

import Distribution.Package -- from Cabal

import Distribution.Solver.Modular.Version
import Distribution.Solver.Types.PackagePath

-- | A package name.
type PN = PackageName

-- | Unpacking a package name.
unPN :: PN -> String
unPN (PackageName pn) = pn

-- | Package version. A package name plus a version number.
type PV = PackageId

-- | Qualified package version.
type QPV = Qualified PV

-- | Package id. Currently just a black-box string.
type PId = UnitId

-- | Location. Info about whether a package is installed or not, and where
-- exactly it is located. For installed packages, uniquely identifies the
-- package instance via its 'PId'.
--
-- TODO: More information is needed about the repo.
data Loc = Inst PId | InRepo
  deriving (Eq, Ord, Show)

-- | Instance. A version number and a location.
data I = I Ver Loc
  deriving (Eq, Ord, Show)

-- | String representation of an instance.
showI :: I -> String
showI (I v InRepo)   = showVer v
showI (I v (Inst uid)) = showVer v ++ "/installed" ++ shortId uid
  where
    -- A hack to extract the beginning of the package ABI hash
    shortId (SimpleUnitId (ComponentId i))
            = snip (splitAt 4) (++ "...")
            . snip ((\ (x, y) -> (reverse x, y)) . break (=='-') . reverse) ('-':)
            $ i
    snip p f xs = case p xs of
                    (ys, zs) -> (if L.null zs then id else f) ys

-- | Package instance. A package name and an instance.
data PI qpn = PI qpn I
  deriving (Eq, Ord, Show, Functor)

-- | String representation of a package instance.
showPI :: PI QPN -> String
showPI (PI qpn i) = showQPN qpn ++ "-" ++ showI i

instI :: I -> Bool
instI (I _ (Inst _)) = True
instI _              = False

-- | Is the package in the primary group of packages. In particular this
-- does not include packages pulled in as setup deps.
--
primaryPP :: PackagePath -> Bool
primaryPP (PackagePath _ns q) = go q
  where
    go Unqualified = True
    go (Base  _)   = True
    go (Setup _)   = False

-- | Create artificial parents for each of the package names, making
-- them all independent.
makeIndependent :: [PN] -> [QPN]
makeIndependent ps = [ Q pp pn | (pn, i) <- zip ps [0::Int ..]
                               , let pp = PackagePath (Independent i) Unqualified
                     ]
