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
  ) where

import Distribution.Setup
import Distribution.Package
import Distribution.Simple.Configure
import Distribution.Simple.Utils

import System.IO
import System.Exit
import System.Cmd (rawSystem)
import Control.Monad

-- -----------------------------------------------------------------------------
-- Build the library

build :: PackageDescription -> LocalBuildInfo -> IO ()
build pkg_descr lbi = do
  setupMessage "Building" pkg_descr

  when (compilerFlavor (compiler lbi) /= GHC) $
	die ("only building with GHC is implemented")
	
  -- first, build the modules
  let args = constructGHCCmdLine pkg_descr lbi
  rawSystemExit (compilerPath (compiler lbi)) args

  -- build any C sources
  when (not (null (cSources pkg_descr))) $
     rawSystemExit (compilerPath (compiler lbi)) (cSources pkg_descr)

  -- now, build the library
  let objs = map (++objsuffix) (allModules pkg_descr)
      lib  = mkLibName (showPackageId (package pkg_descr))
  rawSystemPathExit "ar" (["q", lib] ++ objs)

constructGHCCmdLine :: PackageDescription -> LocalBuildInfo -> [String]
constructGHCCmdLine pkg_descr lbi = 
  [
    "--make",
    "-package-name", showPackageId (package pkg_descr)
  ] 
  ++ extensionsToGHCFlag (extensions pkg_descr)
  ++ [ opt | (GHC,opts) <- options pkg_descr, opt <- opts ]
  ++ allModules pkg_descr

extensionsToGHCFlag _ = [] -- ToDo

#ifdef mingw32_TARGET_OS
objsuffix = ".obj"
#else
objsuffix = ".o"
#endif

mkLibName lib = "libHS" ++ lib ++ ".a"

  -- ToDo: includes, includeDirs
