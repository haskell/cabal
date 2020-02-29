-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build.Macros
-- Copyright   :  Simon Marlow 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Generate cabal_macros.h - CPP macros for package version testing
--
-- When using CPP you get
--
-- > VERSION_<package>
-- > MIN_VERSION_<package>(A,B,C)
--
-- for each /package/ in @build-depends@, which is true if the version of
-- /package/ in use is @>= A.B.C@, using the normal ordering on version
-- numbers.
--
-- TODO Figure out what to do about backpack and internal libraries. It is very
-- suspecious that this stuff works with munged package identifiers
module Distribution.Simple.Build.Macros (
    generateCabalMacrosHeader,
    generatePackageVersionMacros,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Version
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Types
import Distribution.Types.MungedPackageId
import Distribution.Types.MungedPackageName
import Distribution.Types.PackageId
import Distribution.Types.PackageName (unPackageName)
import Distribution.Pretty

import qualified Distribution.Simple.Build.Macros.Z as Z

import Data.ByteString.Builder (Builder)

-- | The contents of the @cabal_macros.h@ for the given configured package.
--
generateCabalMacrosHeader :: PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> Builder
generateCabalMacrosHeader pkg_descr lbi clbi = Z.render Z.Z
    { Z.zPackages        = map mkZPackage $ package pkg_descr : map getPid (componentPackageDeps clbi)
    , Z.zTools           =
        [ Z.ZTool
            { Z.ztoolName    = programId prog
            , Z.ztoolVersion = ver
            , Z.ztoolX       = major1
            , Z.ztoolY       = major2
            , Z.ztoolZ       = minor
            }
        | prog <- configuredPrograms $ withPrograms lbi
        , ver <- maybeToList (programVersion prog)
        , let (major1,major2,minor) = majorMinor ver
        ]
    , Z.zPackageKey      = case clbi of
        LibComponentLocalBuildInfo{} -> componentCompatPackageKey clbi
        _                            -> ""
    , Z.zComponentId     = prettyShow (componentComponentId clbi)
    , Z.zPackageVersion  = pkgVersion (package pkg_descr)
    , Z.zNotNull         = not . null
    , Z.zManglePkgName   = map fixchar . unPackageName
    , Z.zMangleStr       = map fixchar
    }
  where
    getPid (_, MungedPackageId (MungedPackageName pn _) v) =
       -- NB: Drop the library name! We're just reporting package versions.
       -- This would have to be revisited if you are allowed to depend
       -- on different versions of the same package
        PackageIdentifier pn v

-- | Helper function that generates just the @VERSION_pkg@ and @MIN_VERSION_pkg@
-- macros for a list of package ids (usually used with the specific deps of
-- a configured package).
--
generatePackageVersionMacros :: Version -> [PackageId] -> Builder
generatePackageVersionMacros ver pkgids = Z.render Z.Z
    { Z.zPackages        = map mkZPackage pkgids
    , Z.zTools           = []
    , Z.zPackageKey      = ""
    , Z.zComponentId     = ""
    , Z.zPackageVersion  = ver
    , Z.zNotNull         = not . null
    , Z.zManglePkgName   = map fixchar . unPackageName
    , Z.zMangleStr       = map fixchar
    }

mkZPackage :: PackageId -> Z.ZPackage
mkZPackage (PackageIdentifier name ver) = Z.ZPackage
    { Z.zpkgName    = name
    , Z.zpkgVersion = ver
    , Z.zpkgX       = major1
    , Z.zpkgY       = major2
    , Z.zpkgZ       = minor
    }
  where
    (major1,major2,minor) = majorMinor ver

majorMinor :: Version -> (Int, Int, Int)
majorMinor ver = case versionNumbers ver of
        []        -> (0, 0, 0)
        [x]       -> (x, 0, 0)
        [x,y]     -> (x, y, 0)
        (x:y:z:_) -> (x, y, z)

fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c   = c
