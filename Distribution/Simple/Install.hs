{-# OPTIONS -cpp -DDEBUG #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Install
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Explanation: <FIX>
-- WHERE DOES THIS MODULE FIT IN AT A HIGH-LEVEL <FIX>

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

module Distribution.Simple.Install (
	install,
	mkImportDir,
#ifdef DEBUG        
        ,hunitTests
#endif
  ) where

import Distribution.Package (PackageDescription(..), showPackageId)
import Distribution.Simple.Configure(LocalBuildInfo(..))
import Distribution.Simple.Utils(setupMessage, moveSources,
                                 pathSeperatorStr, mkLibName)

import System.Cmd(system)

#ifdef DEBUG
import HUnit (Test)
#endif

-- |FIX: for now, only works with hugs or sdist-style
-- installation... must implement for .hi files and such...  how do we
-- know which files to expect?
install :: FilePath  -- ^build location
        -> PackageDescription -> LocalBuildInfo
        -> Maybe FilePath -- ^install-prefix
        -> IO ()
install buildPref pkg_descr lbi install_prefixM = do
  let pref = (maybe (prefix lbi) id install_prefixM) ++
             pathSeperatorStr ++ "lib" ++ pathSeperatorStr ++ (showPackageId $ package pkg_descr)
  setupMessage "Installing" pkg_descr
  -- FIX: For hugs only
  moveSources "" pref (allModules pkg_descr) (mainModules pkg_descr) ["lhs", "hs"]
  -- FIX: for GHC and NHC only
  moveSources buildPref pref (allModules pkg_descr) (mainModules pkg_descr) ["hi"]
  moveSources buildPref pref (allModules pkg_descr) (mainModules pkg_descr) ["o"]
  system $ "cp " ++ mkLibName buildPref (showPackageId (package pkg_descr))
                 ++ " " ++ mkLibName pref (showPackageId (package pkg_descr))
  return ()
  -- register step should be performed by caller.

-- -----------------------------------------------------------------------------
-- Installation policies

mkImportDir :: PackageDescription -> LocalBuildInfo -> FilePath
mkImportDir pkg_descr lbi = 
#ifdef mingw32_TARGET_OS
	prefix lbi ++ '/':pkg_name
#else
	prefix lbi ++ "/lib/" ++ pkg_name
#endif
  where 
	pkg_name = showPackageId (package pkg_descr)

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
