-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.NHC
-- Copyright   :  Isaac Jones 2003-2006
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--

{- Copyright (c) 2003-2005, Isaac Jones
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

module Distribution.Simple.NHC (
	build{-, install -}
 ) where

import Distribution.PackageDescription
				( PackageDescription(..), BuildInfo(..),
				  Library(..), libModules, hcOptions)
import Distribution.Simple.LocalBuildInfo
				( LocalBuildInfo(..) )
import Distribution.Simple.Utils( rawSystemExit )
import Distribution.Compiler 	( Compiler(..), CompilerFlavor(..),
				  extensionsToNHCFlag )

-- |FIX: For now, the target must contain a main module.  Not used
-- ATM. Re-add later.
build :: PackageDescription -> LocalBuildInfo -> Int -> IO ()
build pkg_descr lbi verbose = do
  -- Unsupported extensions have already been checked by configure
  let flags = snd $ extensionsToNHCFlag (maybe [] (extensions . libBuildInfo) (library pkg_descr))
  rawSystemExit verbose (compilerPath (compiler lbi))
                (["-nhc98"]
                ++ flags
                ++ maybe [] (hcOptions NHC . options . libBuildInfo) (library pkg_descr)
                ++ (libModules pkg_descr))

