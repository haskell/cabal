{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Register
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
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
        installedPkgConfigFile,
#ifdef DEBUG
        hunitTests
#endif
  ) where

import Distribution.Simple.Configure (LocalBuildInfo, compiler)
import Distribution.Setup (CompilerFlavor(..), Compiler(..))
import Distribution.Package (setupMessage, PackageDescription(..), PackageIdentifier(..))
import Distribution.Simple.Utils (rawSystemExit, die)
import Distribution.Simple.GHCPackageConfig (mkGHCPackageConfig, showGHCPackageConfig)
import qualified Distribution.Simple.GHCPackageConfig
    as GHC (localPackageConfig, canWriteLocalPackageConfig, maybeCreateLocalPackageConfig)

import System.Directory(doesFileExist)

import Control.Monad (when, unless)

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
register pkg_descr lbi userInst = do
  setupMessage "Registering" pkg_descr

  case compilerFlavor (compiler lbi) of
   GHC -> do when userInst (GHC.maybeCreateLocalPackageConfig >> return() )
             localConf <- GHC.localPackageConfig
             pkgConfWriteable <- GHC.canWriteLocalPackageConfig
             when (userInst && (not pkgConfWriteable))
                (die ("--user flag passed, but cannot write to local package config: "
                      ++ localConf))
             instConfExists <- doesFileExist installedPkgConfigFile
             unless instConfExists (writeInstalledConfig pkg_descr lbi)
             rawSystemExit (compilerPkgTool (compiler lbi))
	                     (["--auto-ghci-libs", "--update-package",
                               "--input-file="++installedPkgConfigFile]
                              ++ (if userInst
                                  then ["--config-file=" ++ localConf]
                                  else []))
   -- FIX (HUGS):
   Hugs -> setupMessage "Warning: Hugs has no packaging tool\nLibrary files will just be moved into place." pkg_descr
   _   -> die ("only registering with GHC is implemented")

-- |Register doesn't drop the register info file, it must be done in a separate step.
writeInstalledConfig :: PackageDescription -> LocalBuildInfo -> IO ()
writeInstalledConfig pkg_descr lbi = do
  case compilerFlavor (compiler lbi) of
   GHC -> do let pkg_config = mkGHCPackageConfig pkg_descr lbi
             writeFile installedPkgConfigFile (showGHCPackageConfig pkg_config)
   Hugs -> return ()
   _   -> die ("only registering with GHC is implemented")

installedPkgConfigFile :: String
installedPkgConfigFile = ".installed-pkg-config"

-- -----------------------------------------------------------------------------
-- Unregistration

unregister :: PackageDescription -> LocalBuildInfo -> IO ()
unregister pkg_descr lbi = do
  setupMessage "Unregistering" pkg_descr

  when (compilerFlavor (compiler lbi) /= GHC) $
	die ("only unregistering with GHC is implemented")
	
  rawSystemExit (compilerPkgTool (compiler lbi))
	["--remove-package=" ++ pkgName (package pkg_descr)]


-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
