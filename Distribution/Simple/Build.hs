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

import Distribution.Misc (Extension(..), extensionsToNHCFlag, extensionsToGHCFlag)
import Distribution.Setup (CompilerFlavor(..), compilerFlavor, compilerPath)
import Distribution.Package (PackageIdentifier(..), PackageDescription(..),
                             BuildInfo(..), showPackageId, Executable(..), hasLibs)
import Distribution.Simple.Configure (LocalBuildInfo(..), compiler, exeDeps)
import Distribution.Simple.Utils (rawSystemExit, setupMessage,
                                  die, rawSystemPathExit,
                                  split, createIfNotExists,
                                  mkLibName, moveSources, pathJoin, splitExt
                                 )


import Control.Monad (when, unless)
import Data.List(intersperse, nub)
import Data.Maybe(fromJust)
import System.Environment (getEnv)
import qualified Distribution.Simple.GHCPackageConfig as GHC (localPackageConfig)

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
  let (unsupported, flags) = extensionsToNHCFlag (maybe [] extensions (library pkg_descr))
  unless (null unsupported)
           (die $ "Unsupported extension for NHC: "
                  ++ (concat $ intersperse ", " (map show unsupported)))
  rawSystemExit (compilerPath (compiler lbi))
                (["-nhc98"]
                ++ flags
                ++ [ opt | (NHC,opts) <- maybe [] options (library pkg_descr),
                           opt <- opts ]
                ++ maybe [] modules (library pkg_descr))

-- |Building for GHC
buildGHC :: FilePath -> PackageDescription -> LocalBuildInfo -> IO ()
buildGHC pref pkg_descr lbi = do
  let ghcPath = compilerPath (compiler lbi)
  (pkgConf, pkgConfExists) <- GHC.localPackageConfig
  unless pkgConfExists $ writeFile pkgConf "[]\n"
  -- Build lib
  withLib pkg_descr $ \build -> do
      let args = ["-package-conf", pkgConf,
                  "-package-name", pkgName (package pkg_descr),
                  "-odir", pref, "-hidir", pref
                 ]
              ++ constructGHCCmdLine pref build (packageDeps lbi)
              ++ modules build
      unless (null (modules build)) $
        rawSystemExit ghcPath args

      -- build any C sources
      unless (null (cSources build)) $
         rawSystemExit ghcPath (cSources build ++ ["-odir", pref, "-hidir", pref, "-c"])

      let hObjs = map (++objsuffix) (map dotToSep (modules build))
          cObjs = [file ++ objsuffix | (file, _)
                   <- (map splitExt (cSources build))]
          lib  = mkLibName pref (showPackageId (package pkg_descr))
      unless (null hObjs && null cObjs)
        (rawSystemPathExit "ar" (["q", lib] ++ [pathJoin [pref, x] | x <- hObjs ++ cObjs]))

  -- build any executables
  sequence_ [ do let args = ["-package-conf", pkgConf,
                             "-o", pathJoin [pref, exeName]
                            ]
                         ++ constructGHCCmdLine pref exeBi (exeDeps exeName lbi)
                         ++ [pathJoin [pref, modPath]]
                 rawSystemExit ghcPath args
             | Executable exeName modPath exeBi <- executables pkg_descr]

constructGHCCmdLine :: FilePath -> BuildInfo -> [PackageIdentifier] -> [String]
constructGHCCmdLine pref build deps = 
    let (unsupported, flags) = extensionsToGHCFlag (extensions build)
        in if null unsupported
           then [ "--make", "-i" ++ pref ]
                ++ nub (flags ++ [ opt | (GHC,opts) <- options build, opt <- opts ])
                ++ (concat [ ["-package", pkgName pkg] | pkg <- deps ] )
           else error $ "Unsupported extension for GHC: "
                      ++ (concat $ intersperse ", " (map show unsupported))

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
    withLib pkg_descr $ \lib ->
        moveSources (hsSourceDir lib) pref (modules lib) ["hs","lhs"] 
    sequence_ [ moveSources (hsSourceDir exeBi) pref (modules exeBi) ["hs","lhs"]
              | Executable exeName modPath exeBi <- executables pkg_descr]

  -- Todo: includes, includeDirs

-- |If the package description has a library section, call the given
--  function with the library build info as argument.
withLib :: PackageDescription -> (BuildInfo -> IO ()) -> IO ()
withLib pkg_descr f = when (hasLibs pkg_descr) $ f (fromJust (library pkg_descr))

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
