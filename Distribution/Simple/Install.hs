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

import Distribution.Package (PackageDescription(..), BuildInfo(..), Executable(..),
                             showPackageId, hasLibs)
import Distribution.Simple.Configure(LocalBuildInfo(..))
import Distribution.Simple.Utils(setupMessage, moveSources,
                                 mkLibName, pathJoin,
                                 copyFile, die, createIfNotExists
                                )
import Distribution.Setup (CompilerFlavor(..), Compiler(..))

import Control.Monad(when)
import System.Cmd(system)

#ifdef DEBUG
import HUnit (Test)
#endif

-- |FIX: nhc isn't implemented yet.
install :: FilePath  -- ^build location
        -> PackageDescription -> LocalBuildInfo
        -> Maybe FilePath -- ^install-prefix
        -> IO ()
install buildPref pkg_descr lbi install_prefixM = do
  let libPref = mkLibDir pkg_descr lbi install_prefixM
  let binPref = mkBinDir pkg_descr lbi install_prefixM
  setupMessage ("Installing: " ++ libPref ++ "&" ++ binPref) pkg_descr
  case compilerFlavor (compiler lbi) of
     GHC  -> do when (hasLibs pkg_descr) (installLibGHC libPref buildPref pkg_descr)
                installExeGhc binPref buildPref pkg_descr
     Hugs -> do when (hasLibs pkg_descr) (installHugs libPref buildPref pkg_descr)
     _    -> die ("only installing with GHC or Hugs is implemented")
  return ()
  -- register step should be performed by caller.

-- |Install executables for GHC.
installExeGhc :: FilePath -- ^install location
              -> FilePath -- ^Build location
              -> PackageDescription -> IO ()
installExeGhc pref buildPref pkg_descr
    = do createIfNotExists True pref
         sequence_ [copyFile (pathJoin [buildPref, hsSourceDir b, e]) (pathJoin [pref, e])
                    | Executable e _ b <- executables pkg_descr]

-- |Install for ghc, .hi and .a
installLibGHC :: FilePath -- ^install location
              -> FilePath -- ^Build location
              -> PackageDescription -> IO ()
installLibGHC pref buildPref pkg_descr@PackageDescription{library=Just l,
                                                          package=p}
    = do moveSources (pathJoin [buildPref, hsSourceDir l]) pref (modules l) ["hi"]
         copyFile (mkLibName buildPref (showPackageId p))
                    (mkLibName pref (showPackageId p))

-- |Install for hugs, .lhs and .hs
installHugs :: FilePath -- ^Install location
            -> FilePath -- ^Build location
            -> PackageDescription -> IO ()
installHugs pref buildPref pkg_descr@PackageDescription{library=Just l}
    = moveSources (pathJoin [buildPref, hsSourceDir l]) pref (modules l) ["lhs", "hs"]

-- -----------------------------------------------------------------------------
-- Installation policies

mkLibDir :: PackageDescription -> LocalBuildInfo -> Maybe FilePath -> FilePath
mkLibDir pkg_descr lbi install_prefixM = 
	pathJoin [ maybe (prefix lbi) id install_prefixM
#ifndef mingw32_TARGET_OS
                 , "lib"
#endif
	         , showPackageId (package pkg_descr)
	         ]

mkBinDir :: PackageDescription -> LocalBuildInfo -> Maybe FilePath -> FilePath
mkBinDir pkg_descr lbi install_prefixM = 
	pathJoin [(maybe (prefix lbi) id install_prefixM), "bin"]

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
