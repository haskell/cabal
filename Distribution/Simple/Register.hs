-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Register
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC, Hugs
--
-- Explanation: Perform the ".\/setup register" action.  Uses a
-- drop-file for HC-PKG. See also InstalledPackageInfo


{- Copyright (c) 2003-2004, Isaac Jones
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.Register (
	register,
	unregister,
        writeInstalledConfig,
	removeInstalledConfig,
        installedPkgConfigFile,
#ifdef DEBUG
        hunitTests
#endif
  ) where

import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Install (mkLibDir)
import Distribution.Setup (CompilerFlavor(..), Compiler(..))
import Distribution.PackageDescription (setupMessage, PackageDescription(..),
					BuildInfo(..), Library(..))
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.Version (Version(..))
import Distribution.InstalledPackageInfo
	(InstalledPackageInfo, showInstalledPackageInfo, 
	 emptyInstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Simple.Utils (rawSystemExit, die)
import Distribution.Simple.Install (hugsPackageDir, hugsProgramsDir)
import Distribution.Simple.GHCPackageConfig (mkGHCPackageConfig, showGHCPackageConfig)
import qualified Distribution.Simple.GHCPackageConfig
    as GHC (localPackageConfig, canWriteLocalPackageConfig, maybeCreateLocalPackageConfig)
import Distribution.Compat.Directory (copyFile,createDirectoryIfMissing,removeDirectoryRecursive)
import Distribution.Compat.FilePath (joinFileName)

import System.Directory(doesFileExist, removeFile)
import System.IO.Error (try)

import Control.Monad (when, unless)
import Data.Maybe (isNothing, fromJust)

#ifdef DEBUG
import HUnit (Test)
#endif

-- -----------------------------------------------------------------------------
-- Registration

-- |Be sure to call writeInstalledConfig first.  If the --user flag
-- was passed, and ~\/.ghc-packages is writable, or can be created,
-- then we use that file, perhaps creating it.

register :: PackageDescription -> LocalBuildInfo
         -> Bool -- ^Install in the user's database?
         -> IO ()
register pkg_descr lbi userInst
  | isNothing (library pkg_descr) = do
    setupMessage "No package to register" pkg_descr
    return ()
  | otherwise = do
    setupMessage "Registering" pkg_descr
    case compilerFlavor (compiler lbi) of
      GHC -> do 
     	let ghc_63_plus = compilerVersion (compiler lbi) >= Version [6,3] []

	config_flags <-
	   if userInst
		then if ghc_63_plus
			then return ["--user"]
			else do 
			  GHC.maybeCreateLocalPackageConfig
		          localConf <- GHC.localPackageConfig
			  pkgConfWriteable <- GHC.canWriteLocalPackageConfig
		          when (not pkgConfWriteable) $ userPkgConfErr localConf
			  return ["--config-file=" ++ localConf]
		else return []

        instConfExists <- doesFileExist installedPkgConfigFile
        unless instConfExists (writeInstalledConfig pkg_descr lbi)

	let register_flags 
		| ghc_63_plus = ["register", installedPkgConfigFile]
		| otherwise   = ["--update-package",
				 "--input-file="++installedPkgConfigFile]

        rawSystemExit 0 (compilerPkgTool (compiler lbi))
	                     (["--auto-ghci-libs"]
			      ++ register_flags
                              ++ config_flags)
      -- FIX (HUGS):
      Hugs -> do
	createDirectoryIfMissing True (hugsPackageDir pkg_descr lbi)
	copyFile installedPkgConfigFile
	    (hugsPackageDir pkg_descr lbi `joinFileName` "package.conf")
      _   -> die ("only registering with GHC is implemented")

userPkgConfErr local_conf = 
  die ("--user flag passed, but cannot write to local package config: "
    	++ local_conf )

-- |Register doesn't drop the register info file, it must be done in a separate step.
writeInstalledConfig :: PackageDescription -> LocalBuildInfo -> IO ()
writeInstalledConfig pkg_descr lbi = do
  let hc = compiler lbi
  let pkg_config = case compilerFlavor hc of
	GHC | compilerVersion hc < Version [6,3] [] ->
	    showGHCPackageConfig (mkGHCPackageConfig pkg_descr lbi)
	_ -> showInstalledPackageInfo (mkInstalledPackageInfo pkg_descr lbi)
  writeFile installedPkgConfigFile (pkg_config ++ "\n")

removeInstalledConfig :: IO ()
removeInstalledConfig = try (removeFile installedPkgConfigFile) >> return ()

installedPkgConfigFile :: String
installedPkgConfigFile = ".installed-pkg-config"

-- -----------------------------------------------------------------------------
-- Making the InstalledPackageInfo

mkInstalledPackageInfo
	:: PackageDescription
	-> LocalBuildInfo
	-> InstalledPackageInfo
mkInstalledPackageInfo pkg_descr lbi
  = let 
	lib = fromJust (library pkg_descr) -- checked for Nothing earlier
    in
    emptyInstalledPackageInfo{
        IPI.package           = package pkg_descr,
        IPI.license           = license pkg_descr,
        IPI.copyright         = copyright pkg_descr,
        IPI.maintainer        = maintainer pkg_descr,
	IPI.author	      = author pkg_descr,
        IPI.stability         = stability pkg_descr,
	IPI.homepage	      = homepage pkg_descr,
	IPI.pkgUrl	      = pkgUrl pkg_descr,
	IPI.description	      = description pkg_descr,
	IPI.category	      = category pkg_descr,
        IPI.exposed           = True,
	IPI.exposedModules    = exposedModules lib,
	IPI.hiddenModules     = hiddenModules lib,
        IPI.importDirs        = [mkLibDir pkg_descr lbi Nothing],
        IPI.libraryDirs       = [mkLibDir pkg_descr lbi Nothing],
        IPI.hsLibraries       = ["HS" ++ showPackageId (package pkg_descr)],
        IPI.extraLibraries    = extraLibs $ libBuildInfo lib,
        IPI.includeDirs       = includeDirs $ libBuildInfo lib,
        IPI.includes	      = includes $ libBuildInfo lib,
        IPI.depends           = packageDeps lbi,
        IPI.extraHugsOpts     = concat [opts | (Hugs,opts) <- options $ libBuildInfo lib],
        IPI.extraCcOpts       = ccOptions pkg_descr,
        IPI.extraLdOpts       = ldOptions pkg_descr,
        IPI.frameworkDirs     = [],
        IPI.extraFrameworks   = frameworks pkg_descr,
	IPI.haddockInterfaces = [],
	IPI.haddockHTMLs      = []
  }	

-- -----------------------------------------------------------------------------
-- Unregistration

unregister :: PackageDescription -> LocalBuildInfo -> IO ()
unregister pkg_descr lbi = do
  setupMessage "Unregistering" pkg_descr

  case compilerFlavor (compiler lbi) of
    GHC ->
	rawSystemExit 0 (compilerPkgTool (compiler lbi))
	    ["--remove-package=" ++ pkgName (package pkg_descr)]
    Hugs -> do
        try $ removeDirectoryRecursive (hugsPackageDir pkg_descr lbi)
        try $ removeDirectoryRecursive (hugsProgramsDir pkg_descr lbi)
	return ()
    _ ->
	die ("only unregistering with GHC and Hugs is implemented")
	


-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
