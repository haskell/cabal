{-# OPTIONS -cpp #-}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/WorkingConventions#Warnings
-- for details

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

import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription (
	PackageDescription(..), BuildInfo(..), Library(..),
	hasLibs, withLib, hasExes, withExe )
import Distribution.Simple.LocalBuildInfo (
        LocalBuildInfo(..), InstallDirs(..), absoluteInstallDirs, haddockPref)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose,
                                  copyFileVerbose, die,
                                  copyDirectoryRecursiveVerbose)
import Distribution.Simple.Compiler (CompilerFlavor(..), Compiler(..))
import Distribution.Simple.Setup (CopyFlags(..), CopyDest(..))

import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.JHC  as JHC
import qualified Distribution.Simple.Hugs as Hugs

import Control.Monad (when, unless)
import Distribution.Compat.Directory(doesDirectoryExist, doesFileExist)
import System.FilePath(takeDirectory, (</>), isAbsolute)

import Distribution.Verbosity

#ifdef DEBUG
import Test.HUnit (Test)
#endif

-- |Perform the \"@.\/setup install@\" and \"@.\/setup copy@\"
-- actions.  Move files into place based on the prefix argument.  FIX:
-- nhc isn't implemented yet.

install :: PackageDescription -- ^information from the .cabal file
        -> LocalBuildInfo -- ^information from the configure step
        -> CopyFlags -- ^flags sent to copy or install
        -> IO ()
install pkg_descr lbi (CopyFlags copydest verbosity) = do
  let InstallDirs {
         bindir     = binPref,
         libdir     = libPref,
         dynlibdir  = dynlibPref,
         datadir    = dataPref,
         progdir    = progPref,
         docdir     = docPref,
         htmldir    = htmlPref,
         includedir = incPref
      } = absoluteInstallDirs pkg_descr lbi copydest
  docExists <- doesDirectoryExist $ haddockPref pkg_descr
  when (verbosity >= verbose)
       (putStrLn ("directory " ++ haddockPref pkg_descr ++
                  " does exist: " ++ show docExists))
  flip mapM_ (dataFiles pkg_descr) $ \ file -> do
      let dir = takeDirectory file
      createDirectoryIfMissingVerbose verbosity True (dataPref </> dir)
      copyFileVerbose verbosity file (dataPref </> file)
  when docExists $ do
      createDirectoryIfMissingVerbose verbosity True htmlPref
      copyDirectoryRecursiveVerbose verbosity (haddockPref pkg_descr) htmlPref
      -- setPermissionsRecursive [Read] htmlPref

  let lfile = licenseFile pkg_descr
  unless (null lfile) $ do
    createDirectoryIfMissingVerbose verbosity True docPref
    copyFileVerbose verbosity lfile (docPref </> lfile)

  let buildPref = buildDir lbi
  when (hasLibs pkg_descr && verbosity >= normal) $
    putStrLn ("Installing: " ++ libPref)
  when (hasExes pkg_descr && verbosity >= normal) $
    putStrLn ("Installing: " ++ binPref)

  -- install include files for all compilers - they may be needed to compile
  -- haskell files (using the CPP extension)
  when (hasLibs pkg_descr) $ installIncludeFiles verbosity pkg_descr incPref

  case compilerFlavor (compiler lbi) of
     GHC  -> do withLib pkg_descr () $ \_ ->
                  GHC.installLib verbosity lbi libPref dynlibPref buildPref pkg_descr
                withExe pkg_descr $ \_ ->
		  GHC.installExe verbosity binPref buildPref pkg_descr
     JHC  -> do withLib pkg_descr () $ JHC.installLib verbosity libPref buildPref pkg_descr
                withExe pkg_descr $ JHC.installExe verbosity binPref buildPref pkg_descr
     Hugs -> do
       let targetProgPref = progdir (absoluteInstallDirs pkg_descr lbi NoCopyDest)
       let scratchPref = scratchDir lbi
       Hugs.install verbosity libPref progPref binPref targetProgPref scratchPref pkg_descr
     NHC  -> die ("installing with nhc98 is not yet implemented")
     _    -> die ("only installing with GHC, JHC or Hugs is implemented")
  return ()
  -- register step should be performed by caller.

-- | Install the files listed in install-includes
installIncludeFiles :: Verbosity -> PackageDescription -> FilePath -> IO ()
installIncludeFiles verbosity PackageDescription{library=Just l} incdir
 = do
   incs <- mapM (findInc relincdirs) (installIncludes lbi)
   unless (null incs) $ do
     createDirectoryIfMissingVerbose verbosity True incdir
     sequence_ [ copyFileVerbose verbosity path (incdir </> f)
	       | (f,path) <- incs ]
  where
   relincdirs = "." : filter (not.isAbsolute) (includeDirs lbi)
   lbi = libBuildInfo l

   findInc [] f = die ("can't find include file " ++ f)
   findInc (d:ds) f = do
     let path = (d </> f)
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
