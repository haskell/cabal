-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.GHCPackageConfig
-- Copyright   :  (c) The University of Glasgow 2004
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Performs registration for GHC.  Specific to
-- ghc-pkg. Creates a GHC package config file.

module Distribution.Simple.GHCPackageConfig (
	GHCPackageConfig(..),
	mkGHCPackageConfig,
	defaultGHCPackageConfig,
	showGHCPackageConfig,
        localPackageConfig
  ) where

import Distribution.Package (PackageDescription(..), BuildInfo(..), pkgName, showPackageId)
import Distribution.Simple.Configure (LocalBuildInfo(..))
import Distribution.Simple.Install (mkImportDir)
import Distribution.Simple.Utils(pathJoin)

import Text.PrettyPrint.HughesPJ
import System(getEnv)
import System.Directory (doesFileExist)


-- |Where ghc keeps the --user files, the bool is for whether it exists
localPackageConfig :: IO (FilePath, Bool)
localPackageConfig = do u <- getEnv "HOME"
                        let f = pathJoin [u, ".ghc-packages"]
                        b <- doesFileExist f
                        return (f, b)
-- -----------------------------------------------------------------------------
-- GHC 6.2 PackageConfig type

-- Until GHC supports the InstalledPackageInfo type above, we use its
-- existing PackagConfig type.

mkGHCPackageConfig :: PackageDescription -> LocalBuildInfo -> GHCPackageConfig
mkGHCPackageConfig pkg_descr lbi
  = defaultGHCPackageConfig {
	name	        = pkg_name,
	auto	        = False,
	import_dirs     = [mkImportDir pkg_descr lbi],
	library_dirs     = [mkImportDir pkg_descr lbi],
	hs_libraries    = ["HS"++(showPackageId (package pkg_descr))],
	extra_libraries = maybe [] extraLibs (library pkg_descr),
	include_dirs    = maybe [] includeDirs (library pkg_descr),
	c_includes      = maybe [] includes (library pkg_descr),
	package_deps    = map pkgName (packageDeps lbi)
    }
 where
   pkg_name = pkgName (package pkg_descr)

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
