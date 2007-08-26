{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC.PackageConfig
-- Copyright   :  (c) The University of Glasgow 2004
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Performs registration for GHC.  Specific to
-- ghc-pkg. Creates a GHC package config file.  See also
-- 'Distribution.Simple.GHC.build', etc.

module Distribution.Simple.GHC.PackageConfig (
	GHCPackageConfig(..),
	mkGHCPackageConfig,
	defaultGHCPackageConfig,
	showGHCPackageConfig,

        localPackageConfig, maybeCreateLocalPackageConfig,
        canWriteLocalPackageConfig, canReadLocalPackageConfig
  ) where

import Distribution.PackageDescription (PackageDescription(..), BuildInfo(..), Library(..))
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..), absoluteInstallDirs)
import Distribution.Simple.InstallDirs (InstallDirs(..))
import Distribution.Simple.Setup (CopyDest(..))

#ifndef __NHC__
import Control.Exception (try)
#else
import IO (try)
#endif
import Control.Monad(unless)
import Text.PrettyPrint.HughesPJ
import System.Directory (doesFileExist, getPermissions, Permissions (..))
import System.FilePath ((</>))
import Distribution.Compat.Directory (getHomeDirectory)

-- |Where ghc versions < 6.3 keeps the --user files.
-- |return the file, whether it exists, and whether it's readable

localPackageConfig :: IO FilePath
localPackageConfig = do u <- getHomeDirectory
                        return $ (u </> ".ghc-packages")

-- |If the package file doesn't exist, we should try to create it.  If
-- it already exists, do nothing and return true.  This does not take
-- into account whether it is readable or writeable.
maybeCreateLocalPackageConfig :: IO Bool  -- ^success?
maybeCreateLocalPackageConfig
    = do f <- localPackageConfig
         exists <- doesFileExist f
         unless exists $ (try (writeFile f "[]\n") >> return ())
         doesFileExist f


-- |Helper function for canReadPackageConfig and canWritePackageConfig
checkPermission :: (Permissions -> Bool) -> IO Bool
checkPermission perm
    = do f <- localPackageConfig
         exists <- doesFileExist f
         if exists
            then getPermissions f >>= (return . perm)
            else return False

-- |Check for read permission on the localPackageConfig
canReadLocalPackageConfig :: IO Bool
canReadLocalPackageConfig = checkPermission readable

-- |Check for write permission on the localPackageConfig
canWriteLocalPackageConfig :: IO Bool
canWriteLocalPackageConfig = checkPermission writable

-- -----------------------------------------------------------------------------
-- GHC 6.2 PackageConfig type

-- Until GHC supports the InstalledPackageInfo type above, we use its
-- existing PackagConfig type.

mkGHCPackageConfig :: PackageDescription -> LocalBuildInfo -> GHCPackageConfig
mkGHCPackageConfig pkg_descr lbi
  = defaultGHCPackageConfig {
	name	        = pkgName pkg,
	auto	        = True,
	import_dirs     = [libdir installDirs],
	library_dirs    = libdir installDirs
                        : maybe [] (extraLibDirs . libBuildInfo) lib,
	hs_libraries    = ["HS"++(showPackageId (package pkg_descr))],
	extra_libraries = maybe [] (extraLibs   . libBuildInfo) lib,
	include_dirs    = maybe [] (includeDirs . libBuildInfo) lib,
	c_includes      = maybe [] (includes    . libBuildInfo) lib,
	package_deps    = map pkgName (packageDeps lbi)
    }
 where
   pkg = package pkg_descr
   lib = library pkg_descr
   installDirs = absoluteInstallDirs pkg_descr lbi NoCopyDest

data GHCPackageConfig
   = GHCPackage {
	name            :: String,
	auto		:: Bool,
	import_dirs     :: [String],
	source_dirs     :: [String],
	library_dirs    :: [String],
	hs_libraries    :: [String],
	extra_libraries :: [String],
	include_dirs    :: [String],
	c_includes      :: [String],
	package_deps    :: [String],
	extra_ghc_opts  :: [String],
	extra_cc_opts   :: [String],
	extra_ld_opts   :: [String],
	framework_dirs  :: [String], -- ignored everywhere but on Darwin/MacOS X
	extra_frameworks:: [String]  -- ignored everywhere but on Darwin/MacOS X
     }

defaultGHCPackageConfig :: GHCPackageConfig
defaultGHCPackageConfig
   = GHCPackage {
	name = error "defaultPackage",
	auto = False,
	import_dirs     = [],
	source_dirs     = [],
	library_dirs    = [],
	hs_libraries    = [],
	extra_libraries = [],
	include_dirs    = [],
	c_includes      = [],
	package_deps    = [],
	extra_ghc_opts  = [],
	extra_cc_opts   = [],
	extra_ld_opts   = [],
	framework_dirs  = [],
	extra_frameworks= []
    }

-- ---------------------------------------------------------------------------
-- Pretty printing package info

showGHCPackageConfig :: GHCPackageConfig -> String
showGHCPackageConfig pkg = render $
   text "Package" $$ nest 3 (braces (
      sep (punctuate comma [
         text "name = " <> text (show (name pkg)),
	 text "auto = " <> text (show (auto pkg)),
         dumpField "import_dirs"     (import_dirs     pkg),
         dumpField "source_dirs"     (source_dirs     pkg),
         dumpField "library_dirs"    (library_dirs    pkg),
         dumpField "hs_libraries"    (hs_libraries    pkg),
         dumpField "extra_libraries" (extra_libraries pkg),
         dumpField "include_dirs"    (include_dirs    pkg),
         dumpField "c_includes"      (c_includes      pkg),
         dumpField "package_deps"    (package_deps    pkg),
         dumpField "extra_ghc_opts"  (extra_ghc_opts  pkg),
         dumpField "extra_cc_opts"   (extra_cc_opts   pkg),
         dumpField "extra_ld_opts"   (extra_ld_opts   pkg),
         dumpField "framework_dirs"  (framework_dirs   pkg),
         dumpField "extra_frameworks"(extra_frameworks pkg)
      ])))

dumpField :: String -> [String] -> Doc
dumpField name' val = hang (text name' <+> equals) 2  (dumpFieldContents val)

dumpFieldContents :: [String] -> Doc
dumpFieldContents val = brackets (sep (punctuate comma (map (text . show) val)))
