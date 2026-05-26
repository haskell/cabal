{-# LANGUAGE DeriveFunctor #-}
module Distribution.Solver.Modular.Package
  ( I(..)
  , Loc(..)
  , PackageId
  , PackageIdentifier(..)
  , PackageName, mkPackageName, unPackageName
  , PkgconfigName, mkPkgconfigName, unPkgconfigName
  , PI(..)
  , PN
  , QPV
  , instI
  , instUid
  , makeIndependent
  , primaryPP
  , setupPP
  , showI
  , showPI
  , unPN
  ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude

import qualified Data.List as L
import qualified Data.Set as S

import Distribution.Package -- from Cabal
import Distribution.Pretty (prettyShow)

import Distribution.Solver.Modular.Version
import Distribution.Solver.Types.PackagePath


-- | A package name.
type PN = PackageName

-- | Unpacking a package name.
unPN :: PN -> String
unPN = unPackageName

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
data Loc
  = Inst PId
  | InstGroup PId (Set PId)
  | InRepo
  deriving (Eq, Ord, Show)

-- | Instance. A version number and a location.
data I = I Ver Loc
  deriving (Eq, Ord, Show)

-- | String representation of an instance.
showI :: QPN -> I -> String
showI _qpn (I v InRepo)   = showVer v
showI qpn (I v (Inst uid)) =
  let
    uidPrefix = showQPN qpn <> "-" <> showVer v <> "-"
    renderUid u =
      case L.stripPrefix uidPrefix (prettyShow u) of
        Nothing -> showVer v
        Just stripped -> stripped
  in
    showVer v <> "/installed-" <> renderUid uid
showI qpn (I v (InstGroup uid subUids)) =
  let
    uidPrefix = showQPN qpn <> "-" <> showVer v <> "-"
    renderUid u =
      case L.stripPrefix uidPrefix (prettyShow u) of
        Nothing -> prettyShow u
        Just stripped -> stripped
  in
     showI qpn (I v (Inst uid))
  <> " installed package group ["
  <> unwords (map renderUid $ S.toList subUids)
  <> "]"

-- | Package instance. A package name and an instance.
data PI qpn = PI qpn I
  deriving (Eq, Ord, Show, Functor)

-- | String representation of a package instance.
showPI :: PI QPN -> String
showPI (PI qpn i) = showQPN qpn ++ "-" ++ showI qpn i

instI :: I -> Bool
instI (I _ (Inst _)) = True
instI _              = False

instUid :: UnitId -> I -> Bool
instUid uid (I _ (Inst uid')) = uid == uid'
instUid _ _ = False

-- | Is the package in the primary group of packages.  This is used to
-- determine (1) if we should try to establish stanza preferences
-- for this goal, and (2) whether or not a user specified @--constraint@
-- should apply to this dependency (grep 'primaryPP' to see the
-- use sites).  In particular this does not include packages pulled in
-- as setup deps.
--
primaryPP :: PackagePath -> Bool
primaryPP (PackagePath _ns q) = go q
  where
    go QualToplevel    = True
    go (QualBase  _)   = True
    go (QualSetup _)   = False
    go (QualExe _ _)   = False

-- | Is the package a dependency of a setup script.  This is used to
-- establish whether or not certain constraints should apply to this
-- dependency (grep 'setupPP' to see the use sites).
--
setupPP :: PackagePath -> Bool
setupPP (PackagePath _ns (QualSetup _)) = True
setupPP (PackagePath _ns _)         = False

-- | Qualify a target package with its own name so that its dependencies are not
-- required to be consistent with other targets.
makeIndependent :: PN -> QPN
makeIndependent pn = Q (PackagePath (Independent pn) QualToplevel) pn
