{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Install
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Explanation: Perform the ".\/setup install" action.  Move files into
-- place based on the prefix argument.

{- All rights reserved.

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

module Distribution.Simple.Install (
	install,
	mkBinDir,
	mkLibDir,
#ifdef DEBUG        
        hunitTests
#endif
  ) where

#if __GLASGOW_HASKELL__ < 603 
#include "config.h"
#endif

import Distribution.PackageDescription (
	PackageDescription(..), BuildInfo(..), Executable(..),
	setupMessage, hasLibs, withLib)
import Distribution.Package (showPackageId)
import Distribution.Simple.Configure(LocalBuildInfo(..))
import Distribution.Simple.Utils(moveSources, rawSystemExit,
                                 mkLibName,
                                 die, createIfNotExists
                                )
import Distribution.Setup (CompilerFlavor(..), Compiler(..))

import Control.Monad(when)
import Data.Maybe(maybeToList, fromMaybe)
import Distribution.Compat.Directory(copyFile)
import Distribution.Compat.FilePath(joinFileName)

#ifdef DEBUG
import HUnit (Test)
#endif

-- |FIX: nhc isn't implemented yet.
install :: FilePath  -- ^build location
        -> PackageDescription -> LocalBuildInfo
        -> Maybe FilePath -- ^install-prefix
        -> Bool -- ^Install for user?
        -> IO ()
install buildPref pkg_descr lbi install_prefixM uInst = do
  let libPref = mkLibDir pkg_descr lbi install_prefixM
  let binPref = mkBinDir pkg_descr lbi install_prefixM
  setupMessage ("Installing: " ++ libPref ++ " & " ++ binPref) pkg_descr
  case compilerFlavor (compiler lbi) of
     GHC  -> do when (hasLibs pkg_descr) (installLibGHC libPref buildPref pkg_descr)
                installExeGhc binPref buildPref pkg_descr
     Hugs -> do -- FIX (HUGS): fix 'die' checks commands below.
                when uInst (die "Hugs cannot yet install user-only packages.")
                withLib pkg_descr (\buildInfo@BuildInfo{hsSourceDir=srcDir} ->
                                     do let targetDir = buildPref `joinFileName` srcDir
                                        let args = targetDir
                                                    : (maybeToList install_prefixM)
                                        let hugsPkg = compilerPkgTool $ compiler $ lbi
                                        rawSystemExit hugsPkg args)
                -- FIX (HUGS): Install executables, still needs work in build step
     _    -> die ("only installing with GHC or Hugs is implemented")
  return ()
  -- register step should be performed by caller.

-- |Install executables for GHC.
installExeGhc :: FilePath -- ^install location
              -> FilePath -- ^Build location
              -> PackageDescription -> IO ()
installExeGhc pref buildPref pkg_descr
    = do createIfNotExists True pref
         sequence_ [copyFile (buildPref `joinFileName` (hsSourceDir b) `joinFileName` e) (pref `joinFileName` e)
                    | Executable e _ b <- executables pkg_descr]

-- |Install for ghc, .hi and .a
installLibGHC :: FilePath -- ^install location
              -> FilePath -- ^Build location
              -> PackageDescription -> IO ()
installLibGHC pref buildPref PackageDescription{library=Just l,
                                                package=p}
    = do moveSources (buildPref `joinFileName` (hsSourceDir l)) pref (modules l) ["hi"]
         copyFile (mkLibName buildPref (showPackageId p))
                    (mkLibName pref (showPackageId p))

-- |Install for hugs, .lhs and .hs
installHugs :: FilePath -- ^Install location
            -> FilePath -- ^Build location
            -> PackageDescription -> IO ()
installHugs pref buildPref PackageDescription{library=Just l}
    = moveSources (buildPref `joinFileName` (hsSourceDir l)) pref (modules l) ["lhs", "hs"]

-- -----------------------------------------------------------------------------
-- Installation policies

mkLibDir :: PackageDescription -> LocalBuildInfo -> Maybe FilePath -> FilePath
mkLibDir pkg_descr lbi install_prefixM = 
  (fromMaybe (prefix lbi) install_prefixM) `joinFileName`
#ifndef mingw32_TARGET_OS
                 "lib" `joinFileName`
#endif
	         showPackageId (package pkg_descr)

mkBinDir :: PackageDescription -> LocalBuildInfo -> Maybe FilePath -> FilePath
mkBinDir _ lbi install_prefixM = 
  (fromMaybe (prefix lbi) install_prefixM) `joinFileName` "bin"

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
