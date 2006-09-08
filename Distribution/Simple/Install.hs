{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Install
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Perform the \"@.\/setup install@\" action.  Move files into
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
#ifdef DEBUG        
        hunitTests
#endif
  ) where

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 604
#if __GLASGOW_HASKELL__ < 603
#include "config.h"
#else
#include "ghcconfig.h"
#endif
#endif

import Distribution.PackageDescription (
	PackageDescription(..),
	setupMessage, hasLibs, withLib, withExe )
import Distribution.Simple.LocalBuildInfo (
        LocalBuildInfo(..), mkLibDir, mkBinDir, mkDataDir, mkProgDir, mkHaddockDir)
import Distribution.Simple.Utils(copyFileVerbose, die, haddockPref,  copyDirectoryRecursiveVerbose)
import Distribution.Compiler (CompilerFlavor(..), Compiler(..))
import Distribution.Setup (CopyFlags(..), CopyDest(..))

import Distribution.Compat.Directory(createDirectoryIfMissing)
import Distribution.Compat.FilePath(splitFileName,joinFileName)

import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.JHC  as JHC
-- import qualified Distribution.Simple.NHC  as NHC
import qualified Distribution.Simple.Hugs as Hugs

import Control.Monad(when)
import Distribution.Compat.Directory(createDirectoryIfMissing, doesDirectoryExist)
import Distribution.Compat.FilePath(splitFileName,joinFileName)

#ifdef DEBUG
import HUnit (Test)
#endif

-- |FIX: nhc isn't implemented yet.
install :: PackageDescription
        -> LocalBuildInfo
        -> CopyFlags
        -> IO ()
install pkg_descr lbi (CopyFlags copydest verbose) = do
  let dataFilesExist = not (null (dataFiles pkg_descr))
  docExists <- doesDirectoryExist haddockPref
  when (verbose >= 4)
       (putStrLn ("directory " ++ haddockPref ++
                  " does exist: " ++ show docExists))
  when (dataFilesExist || docExists) $ do
    let dataPref = mkDataDir pkg_descr lbi copydest
    createDirectoryIfMissing True dataPref
    flip mapM_ (dataFiles pkg_descr) $ \ file -> do
      let (dir, _) = splitFileName file
      createDirectoryIfMissing True (dataPref `joinFileName` dir)
      copyFileVerbose verbose file (dataPref `joinFileName` file)
    when docExists $ do
      let targetDir = mkHaddockDir pkg_descr lbi copydest
      createDirectoryIfMissing True targetDir
      copyDirectoryRecursiveVerbose verbose haddockPref targetDir
      -- setPermissionsRecursive [Read] targetDir
  let buildPref = buildDir lbi
  let libPref = mkLibDir pkg_descr lbi copydest
  let binPref = mkBinDir pkg_descr lbi copydest
  setupMessage ("Installing: " ++ libPref ++ " & " ++ binPref) pkg_descr
  case compilerFlavor (compiler lbi) of
     GHC  -> do when (hasLibs pkg_descr)
                     (GHC.installLib verbose (withPrograms lbi)
                       (withVanillaLib lbi) (withProfLib lbi)
                       (withGHCiLib lbi) libPref buildPref pkg_descr)
                GHC.installExe verbose binPref buildPref pkg_descr
     JHC  -> do withLib pkg_descr () $ JHC.installLib verbose libPref buildPref pkg_descr
                withExe pkg_descr $ JHC.installExe verbose binPref buildPref pkg_descr
     Hugs -> do
       let progPref = mkProgDir pkg_descr lbi copydest
       let targetProgPref = mkProgDir pkg_descr lbi NoCopyDest
       Hugs.install verbose libPref progPref binPref targetProgPref buildPref pkg_descr
     _    -> die ("only installing with GHC, JHC or Hugs is implemented")
  return ()
  -- register step should be performed by caller.

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
