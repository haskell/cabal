{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Compiler
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Haskell implementations.

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

module Distribution.Simple.Compiler (
        -- * Haskell implementations
	module Distribution.Compiler,
	Compiler(..),
        showCompilerId, compilerVersion,

        -- * Support for package databases
        PackageDB(..),

        -- * Support for language extensions
        Flag,
        extensionsToFlags,
        unsupportedExtensions
#ifdef DEBUG
        ,hunitTests
#endif
  ) where

import Distribution.Compiler
import Distribution.Version (Version(..))
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Language.Haskell.Extension (Extension(..))

import Data.List (nub)
import Data.Maybe (catMaybes, isNothing)

#ifdef DEBUG
import Test.HUnit (Test)
#endif

data Compiler = Compiler {
        compilerFlavor          :: CompilerFlavor,
        compilerId              :: PackageIdentifier,
	compilerExtensions      :: [(Extension, Flag)]
    }
    deriving (Show, Read)

showCompilerId :: Compiler -> String
showCompilerId = showPackageId . compilerId

compilerVersion :: Compiler -> Version
compilerVersion = pkgVersion . compilerId

-- ------------------------------------------------------------
-- * Package databases
-- ------------------------------------------------------------

-- |Some compilers have a notion of a database of available packages.
-- For some there is just one global db of packages, other compilers
-- support a per-user or an arbitrary db specified at some location in
-- the file system. This can be used to build isloated environments of
-- packages, for example to build a collection of related packages
-- without installing them globally.
data PackageDB = GlobalPackageDB
               | UserPackageDB
               | SpecificPackageDB FilePath
    deriving (Show, Read)

-- ------------------------------------------------------------
-- * Extensions
-- ------------------------------------------------------------

-- |For the given compiler, return the flags for the supported extensions.
unsupportedExtensions :: Compiler -> [Extension] -> [Extension]
unsupportedExtensions comp exts =
  [ ext | ext <- exts
        , isNothing $ lookup ext (compilerExtensions comp) ]

type Flag = String

-- |For the given compiler, return the flags for the supported extensions.
extensionsToFlags :: Compiler -> [Extension] -> [Flag]
extensionsToFlags comp exts =
  nub $ filter (not . null) $ catMaybes
  [ lookup ext (compilerExtensions comp)
  | ext <- exts ]

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
