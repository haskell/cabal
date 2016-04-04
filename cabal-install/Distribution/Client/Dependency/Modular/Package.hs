{-# LANGUAGE DeriveFunctor #-}
module Distribution.Client.Dependency.Modular.Package
  ( I(..)
  , Loc(..)
  , PackageId
  , PackageIdentifier(..)
  , PackageName(..)
  , PI(..)
  , PN
  , PP(..)
  , Namespace(..)
  , Qualifier(..)
  , QPN
  , QPV
  , Q(..)
  , instI
  , makeIndependent
  , primaryPP
  , showI
  , showPI
  , showQPN
  , unPN
  ) where

import Data.List as L

import Distribution.Package -- from Cabal
import Distribution.Text    -- from Cabal

import Distribution.Client.Dependency.Modular.Version

-- | A package name.
type PN = PackageName

-- | Unpacking a package name.
unPN :: PN -> String
unPN (PackageName pn) = pn

-- | Package version. A package name plus a version number.
type PV = PackageId

-- | Qualified package version.
type QPV = Q PV

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

-- | A package path consists of a namespace and a package path inside that
-- namespace.
data PP = PP Namespace Qualifier
  deriving (Eq, Ord, Show)

-- | Top-level namespace
--
-- Package choices in different namespaces are considered completely independent
-- by the solver.
data Namespace =
    -- | The default namespace
    DefaultNamespace

    -- | Independent namespace
    --
    -- For now we just number these (rather than giving them more structure).
  | Independent Int
  deriving (Eq, Ord, Show)

-- | Qualifier of a package within a namespace (see 'PP')
data Qualifier =
    -- | Top-level dependency in this namespace
    Unqualified

    -- | Any dependency on base is considered independent
    --
    -- This makes it possible to have base shims.
  | Base PN

    -- | Setup dependency
    --
    -- By rights setup dependencies ought to be nestable; after all, the setup
    -- dependencies of a package might themselves have setup dependencies, which
    -- are independent from everything else. However, this very quickly leads to
    -- infinite search trees in the solver. Therefore we limit ourselves to
    -- a single qualifier (within a given namespace).
  | Setup PN
  deriving (Eq, Ord, Show)

-- | Is the package in the primary group of packages. In particular this
-- does not include packages pulled in as setup deps.
--
primaryPP :: PP -> Bool
primaryPP (PP _ns q) = go q
  where
    go Unqualified = True
    go (Base  _)   = True
    go (Setup _)   = False

-- | String representation of a package path.
--
-- NOTE: The result of 'showPP' is either empty or results in a period, so that
-- it can be prepended to a package name.
showPP :: PP -> String
showPP (PP ns q) =
    case ns of
      DefaultNamespace -> go q
      Independent i    -> show i ++ "." ++ go q
  where
    -- Print the qualifier
    --
    -- NOTE: the base qualifier is for a dependency _on_ base; the qualifier is
    -- there to make sure different dependencies on base are all independent.
    -- So we want to print something like @"A.base"@, where the @"A."@ part
    -- is the qualifier and @"base"@ is the actual dependency (which, for the
    -- 'Base' qualifier, will always be @base@).
    go Unqualified = ""
    go (Setup pn)  = display pn ++ "-setup."
    go (Base  pn)  = display pn ++ "."

-- | A qualified entity. Pairs a package path with the entity.
data Q a = Q PP a
  deriving (Eq, Ord, Show)

-- | Standard string representation of a qualified entity.
showQ :: (a -> String) -> (Q a -> String)
showQ showa (Q pp x) = showPP pp ++ showa x

-- | Qualified package name.
type QPN = Q PN

-- | String representation of a qualified package path.
showQPN :: QPN -> String
showQPN = showQ display

-- | Create artificial parents for each of the package names, making
-- them all independent.
makeIndependent :: [PN] -> [QPN]
makeIndependent ps = [ Q pp pn | (pn, i) <- zip ps [0::Int ..]
                               , let pp = PP (Independent i) Unqualified
                     ]
