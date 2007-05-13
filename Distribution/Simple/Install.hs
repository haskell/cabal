{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Install
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Perform the \"@.\/setup install@\" and \"@.\/setup
-- copy@\" actions.  Move files into place based on the prefix
-- argument.

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
	PackageDescription(..), BuildInfo(..), Library(..),
	setupMessage, hasLibs, withLib, withExe )
import Distribution.Simple.LocalBuildInfo (
        LocalBuildInfo(..), mkLibDir, mkBinDir, mkDataDir, mkProgDir,
        mkHaddockDir, mkIncludeDir, haddockPref)
import Distribution.Simple.Utils(copyFileVerbose, die, copyDirectoryRecursiveVerbose)
import Distribution.Compiler (CompilerFlavor(..), Compiler(..))
import Distribution.Setup (CopyFlags(..), CopyDest(..))

import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.JHC  as JHC
import qualified Distribution.Simple.NHC  as NHC
import qualified Distribution.Simple.Hugs as Hugs

import Control.Monad(when)
import Distribution.Compat.Directory(createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import Distribution.Compat.FilePath(splitFileName,joinFileName, isAbsolutePath)

#ifdef DEBUG
import HUnit (Test)
#endif

-- |Perform the \"@.\/setup install@\" and \"@.\/setup copy@\"
-- actions.  Move files into place based on the prefix argument.  FIX:
-- nhc isn't implemented yet.

install :: PackageDescription -- ^information from the .cabal file
        -> LocalBuildInfo -- ^information from the configure step
        -> CopyFlags -- ^flags sent to copy or install
        -> IO ()
install pkg_descr lbi (CopyFlags copydest verbose) = do
  let dataFilesExist = not (null (dataFiles pkg_descr))
  docExists <- doesDirectoryExist $ haddockPref pkg_descr
  when (verbose >= 4)
       (putStrLn ("directory " ++ haddockPref pkg_descr ++
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
      copyDirectoryRecursiveVerbose verbose (haddockPref pkg_descr) targetDir
      -- setPermissionsRecursive [Read] targetDir
  let buildPref = buildDir lbi
  let libPref = mkLibDir pkg_descr lbi copydest
  let binPref = mkBinDir pkg_descr lbi copydest
  setupMessage verbose ("Installing: " ++ libPref ++ " & " ++ binPref) pkg_descr

  -- install include files for all compilers - they may be needed to compile
  -- haskell files (using the CPP extension)
  when (hasLibs pkg_descr) $ installIncludeFiles verbose pkg_descr libPref

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
       let scratchPref = scratchDir lbi
       Hugs.install verbose libPref progPref binPref targetProgPref scratchPref pkg_descr
     NHC  -> die ("installing with nhc98 is not yet implemented")
     _    -> die ("only installing with GHC, JHC or Hugs is implemented")
  return ()
  -- register step should be performed by caller.

-- | Install the files listed in install-includes
installIncludeFiles :: Int -> PackageDescription -> FilePath -> IO ()
installIncludeFiles verbose PackageDescription{library=Just l} theLibdir
 = do
   createDirectoryIfMissing True incdir
   incs <- mapM (findInc relincdirs) (installIncludes lbi)
   sequence_ [ copyFileVerbose verbose path (incdir `joinFileName` f)
	     | (f,path) <- incs ]
  where
   relincdirs = filter (not.isAbsolutePath) (includeDirs lbi)
   lbi = libBuildInfo l
   incdir = mkIncludeDir theLibdir

   findInc [] f = die ("can't find include file " ++ f)
   findInc (d:ds) f = do
     let path = (d `joinFileName` f)
     b <- doesFileExist path
     if b then return (f,path) else findInc ds f
installIncludeFiles _ _ _ = die "installIncludeFiles: Can't happen?"

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
