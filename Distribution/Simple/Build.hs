{-# OPTIONS -cpp #-}
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

import Distribution.Misc (extensionsToGHCFlag, extensionsToNHCFlag)
import Distribution.Setup (Compiler(..), CompilerFlavor(..))
import Distribution.Package (PackageIdentifier(..), PackageDescription(..),
                             BuildInfo(..), showPackageId, setupMessage,
                             withLib, Executable(..))
import Distribution.PreProcess (preprocessSources, PPSuffixHandler)
import Distribution.Simple.Configure (LocalBuildInfo(..), compiler, exeDeps)
import Distribution.Simple.Utils (rawSystemExit, die, rawSystemPathExit,
                                  split, createIfNotExists,
                                  mkLibName, pathJoin, moveSources,
                                  splitFilePath, joinFilenameDir, joinExt
                                 )


import Control.Monad (unless, when)
import Control.Exception (try)
import Data.List(nub)
import System.Directory (removeFile)
import qualified Distribution.Simple.GHCPackageConfig
    as GHC (localPackageConfig, canReadLocalPackageConfig)

#ifdef DEBUG
import HUnit (Test)
#endif

-- -----------------------------------------------------------------------------
-- Build the library

build :: FilePath -- ^Build location
         -> PackageDescription
         -> LocalBuildInfo
         -> [ PPSuffixHandler ]
         -> IO ()
build pref pkg_descr lbi suffixes = do
  createIfNotExists True pref
  preprocessSources pkg_descr lbi suffixes
  setupMessage "Building" pkg_descr
  case compilerFlavor (compiler lbi) of
   GHC -> buildGHC pref pkg_descr lbi
   Hugs -> buildHugs pref pkg_descr lbi
   _   -> die ("Only building with GHC and preprocessing for hugs are implemented.")

-- |FIX: For now, the target must contain a main module.  Not used
-- ATM. Re-add later.
buildNHC :: PackageDescription -> LocalBuildInfo -> IO ()
buildNHC pkg_descr lbi = do
  -- Unsupported extensions have already been checked by configure
  let flags = snd $ extensionsToNHCFlag (maybe [] extensions (library pkg_descr))
  rawSystemExit (compilerPath (compiler lbi))
                (["-nhc98"]
                ++ flags
                ++ [ opt | (NHC,opts) <- maybe [] options (library pkg_descr),
                           opt <- opts ]
                ++ maybe [] modules (library pkg_descr))

-- |Building for GHC.  If .ghc-packages exists and is readable, add
-- it to the command-line.
buildGHC :: FilePath -> PackageDescription -> LocalBuildInfo -> IO ()
buildGHC pref pkg_descr lbi = do
  let ghcPath = compilerPath (compiler lbi)
  pkgConf <- GHC.localPackageConfig
  pkgConfReadable <- GHC.canReadLocalPackageConfig
  -- Build lib
  withLib pkg_descr $ \buildInfo' -> do
      createIfNotExists True (pathJoin [pref, hsSourceDir buildInfo'])
      let args = (if pkgConfReadable then ["-package-conf", pkgConf] else [])
              ++ ["-package-name", pkgName (package pkg_descr),
                  "-odir", pathJoin [pref, hsSourceDir buildInfo'],
                  "-hidir", pathJoin [pref, hsSourceDir buildInfo']
                 ]
              ++ constructGHCCmdLine buildInfo' (packageDeps lbi)
              ++ modules buildInfo'
      unless (null (modules buildInfo')) $
        rawSystemExit ghcPath args

      -- build any C sources
      unless (null (cSources buildInfo')) $
         rawSystemExit ghcPath (cSources buildInfo' ++ ["-odir", pref, "-hidir", pref, "-c"])

      let hObjs = [ pathJoin [hsSourceDir buildInfo', dotToSep x `joinExt` objsuffix]
                  | x <- modules buildInfo' ]
          cObjs = [ path `joinFilenameDir` file `joinExt` objsuffix
                  | (path, file, _) <- (map splitFilePath (cSources buildInfo')) ]
          lib  = mkLibName pref (showPackageId (package pkg_descr))
      unless (null hObjs && null cObjs) $ do
        try (removeFile lib) -- first remove library if it exists
        rawSystemPathExit "ar" (["q", lib] ++ [pathJoin [pref, x] | x <- hObjs ++ cObjs])

  -- build any executables
  sequence_ [ do createIfNotExists True (pathJoin [pref, hsSourceDir exeBi])
                 let args = (if pkgConfReadable then ["-package-conf", pkgConf] else [])
                         ++ ["-odir", pathJoin [pref, hsSourceDir exeBi],
                             "-hidir", pathJoin [pref, hsSourceDir exeBi],
                             "-o", pathJoin [pref, hsSourceDir exeBi, exeName']
                            ]
                         ++ constructGHCCmdLine exeBi (exeDeps exeName' lbi)
                         ++ [pathJoin [hsSourceDir exeBi, modPath]]
                 rawSystemExit ghcPath args
             | Executable exeName' modPath exeBi <- executables pkg_descr]

constructGHCCmdLine :: BuildInfo -> [PackageIdentifier] -> [String]
constructGHCCmdLine buildInfo' deps = 
    -- Unsupported extensions have already been checked by configure
    let flags = snd $ extensionsToGHCFlag (extensions buildInfo')
     in [ "--make", "-i" ++ hsSourceDir buildInfo' ]
     ++ nub (flags ++ [ opt | (GHC,opts) <- options buildInfo', opt <- opts ])
     ++ (concat [ ["-package", pkgName pkg] | pkg <- deps ])

-- |
buildHugs :: FilePath -> PackageDescription -> LocalBuildInfo -> IO ()
buildHugs pref pkg_descr lbi
    = do -- move library-related source files into place.
         withLib pkg_descr (\buildInfo@BuildInfo{hsSourceDir=srcDir, modules=mods} -> 
                               do let targetDir = pathJoin [pref, hsSourceDir buildInfo]
                                  moveSources srcDir pref mods ["hs", "lhs"]
                           )


         {- FIX (HUGS): something smart must happen here; we need to
            create a shell script or bat file or something to call
            runhugs for running the executable file.  For now, ignore
            executables -}
         when (not $ null $ executables $ pkg_descr)
              (setupMessage "Warning: executable stanzas ignored for HUGS.\nNot yet implemented in cabal." pkg_descr)
         return ()


objsuffix :: String
objsuffix = "o"

dotToSep :: String -> String
dotToSep s = pathJoin (split '.' s)

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
