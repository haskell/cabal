{-# OPTIONS -cpp -DDEBUG #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
--

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

module Distribution.Simple.Build (
	build
#ifdef DEBUG        
        ,hunitTests
#endif
  ) where

import Distribution.Misc (Extension(..))
import Distribution.Setup (CompilerFlavor(..), compilerFlavor, compilerPath)
import Distribution.Package (PackageDescription(..), showPackageId)
import Distribution.Simple.Configure (LocalBuildInfo(..), compiler)
import Distribution.Simple.Utils (rawSystemExit, setupMessage,
                                  die, rawSystemPathExit,
                                  split, createIfNotExists,
                                  mkLibName, moveSources, pathJoin
                                 )


import Control.Monad (when)
import Data.List(intersperse, nub)

#ifdef DEBUG
import HUnit (Test)
#endif

-- -----------------------------------------------------------------------------
-- Build the library

build :: FilePath -- ^Build location
         -> PackageDescription -> LocalBuildInfo -> IO ()
build pref pkg_descr lbi = do
  createIfNotExists True pref
  preprocessSources pkg_descr lbi pref
  setupMessage "Building" pkg_descr
  case compilerFlavor (compiler lbi) of
   GHC -> buildGHC pref pkg_descr lbi
   NHC -> buildNHC pkg_descr lbi
   Hugs -> return ()
   _   -> die ("building with GHC & NHC is implemented, preprocessing for hugs.")

-- |FIX: For now, the target must contain a main module :(
buildNHC :: PackageDescription -> LocalBuildInfo -> IO ()
buildNHC pkg_descr lbi = do
  rawSystemExit (compilerPath (compiler lbi))
                (["-nhc98"]
                ++ extensionsToNHCFlag (extensions pkg_descr)
                ++ [ opt | (NHC,opts) <- options pkg_descr, opt <- opts ]
                ++ allModules pkg_descr)

-- |Building for GHC
buildGHC :: FilePath -> PackageDescription -> LocalBuildInfo -> IO ()
buildGHC pref pkg_descr lbi = do

  -- first, build the modules
  let args = constructGHCCmdLine pref pkg_descr lbi
  rawSystemExit (compilerPath (compiler lbi)) args

  -- build any C sources
  when (not (null (cSources pkg_descr))) $
     rawSystemExit (compilerPath (compiler lbi)) (cSources pkg_descr)

  -- now, build the library
  let objs = map (++objsuffix) (map dotToSep (allModules pkg_descr))
      lib  = mkLibName pref (showPackageId (package pkg_descr))
  rawSystemPathExit "ar" (["q", lib] ++ [pathJoin [pref, x] | x <- objs])

constructGHCCmdLine :: FilePath -> PackageDescription -> LocalBuildInfo -> [String]
constructGHCCmdLine pref pkg_descr lbi = 
  [
    "--make", "-odir " ++ pref, "-hidir " ++ pref,
    "-package-name", showPackageId (package pkg_descr)
  ] 
  ++ extensionsToGHCFlag (extensions pkg_descr)
  ++ [ opt | (GHC,opts) <- options pkg_descr, opt <- opts ]
  ++ [ "-i" ++ pref ]
  ++ [ "-package " ++ showPackageId pkg | pkg <- packageDeps lbi ] 
  ++ allModules pkg_descr

extensionsToGHCFlag :: [ Extension ] -> [String]
extensionsToGHCFlag = nub . map extensionToGHCFlag
    where
    extensionToGHCFlag OverlappingInstances = "-fallow-overlapping-instances"
    extensionToGHCFlag TypeSynonymInstances = "-fglasgow-exts"
    extensionToGHCFlag TemplateHaskell = "-fth"

extensionsToNHCFlag :: [ Extension ] -> [String]
extensionsToNHCFlag _ = [] -- ToDo

objsuffix :: String
#ifdef mingw32_TARGET_OS
objsuffix = ".obj"
#else
objsuffix = ".o"
#endif

dotToSep :: String -> String
dotToSep s = pathJoin (split '.' s)

-- |Copy and (possibly) preprocess sources from hsSourceDirs
preprocessSources :: PackageDescription 
		  -> LocalBuildInfo 
		  -> FilePath           -- ^ Directory to put preprocessed 
					-- sources in
		  -> IO ()
preprocessSources pkg_descr lbi pref = 
    do
    setupMessage "Preprocessing" pkg_descr
    putStrLn (hsSourceDir pkg_descr)
    moveSources (hsSourceDir pkg_descr) pref 
		    (allModules pkg_descr) (mainModules pkg_descr)
                    ["hs","lhs"] 

  -- Todo: includes, includeDirs

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
