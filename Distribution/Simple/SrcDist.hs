-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.SrcDist
-- Copyright   :  Simon Marlow 2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
--

{- Copyright (c) 2003-2004, Simon Marlow
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

module Distribution.Simple.SrcDist (
	sdist
  )  where

import Distribution.Package(PackageDescription(..), showPackageId)
import Distribution.Simple.Configure(LocalBuildInfo)
import Distribution.Simple.Utils(setupMessage, moveSources, pathSeperatorStr)

import System.Cmd (system)

-- |Create a source distribution. FIX: Calls tar directly (won't work
-- on windows).
sdist :: PackageDescription -> LocalBuildInfo -> IO ()
sdist pkg_descr _ = do
  setupMessage "Building source dist for" pkg_descr
  moveSources (distSrc++pathSeperatorStr++nameVersion pkg_descr)
              (allModules pkg_descr) (mainModules pkg_descr)
  system $ "tar --directory=" ++ distSrc ++ " -zcf"
	     ++ " dist/" ++ (tarBallName pkg_descr)
		    ++ " " ++ (nameVersion pkg_descr)
  system $ "rm -rf " ++ distSrc
  putStrLn "Source tarball created."

------------------------------------------------------------

distSrc :: FilePath
distSrc = "dist/src"

-- |The file name of the tarball
tarBallName :: PackageDescription -> FilePath
tarBallName p = (nameVersion p) ++ ".tgz"

nameVersion :: PackageDescription -> String
nameVersion = showPackageId . package
