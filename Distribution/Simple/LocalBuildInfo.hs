-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.LocalBuildInfo
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Definition of the LocalBuildInfo data type.

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

module Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
    where

import Distribution.Package (PackageIdentifier)
import Distribution.Setup (Compiler)

-- |Data cached after configuration step.
data LocalBuildInfo = LocalBuildInfo {
  	prefix	    :: FilePath,
		-- ^ The installation directory (eg. @/usr/local@, or
		-- @C:/Program Files/foo-1.2@ on Windows.
	compiler    :: Compiler,
		-- ^ The compiler we're building with
	buildDir    :: FilePath,
		-- ^ Where to put the result of building.
	packageDeps :: [PackageIdentifier],
		-- ^ Which packages we depend on, /exactly/.
		-- The 'Distribution.PackageDescription.PackageDescription'
		-- specifies a set of build dependencies
		-- that must be satisfied in terms of version ranges.  This
		-- field fixes those dependencies to the specific versions
		-- available on this machine for this compiler.
        withHaddock :: Maybe FilePath, -- ^Might be the location of the Haddock executable.
        withHappy   :: Maybe FilePath, -- ^Might be the location of the Happy executable.
        withAlex    :: Maybe FilePath, -- ^Might be the location of the Alex executable.
        withHsc2hs  :: Maybe FilePath, -- ^Might be the location of the Hsc2hs executable.
        withCpphs   :: Maybe FilePath  -- ^Might be the location of the Cpphs executable.
  }
  deriving (Show, Read, Eq)

