-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.BuildPaths
-- Copyright   :  Isaac Jones 2003-2004,
--                Duncan Coutts 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A bunch of dirs, paths and file names used for intermediate build steps.
--

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

module Distribution.Simple.BuildPaths (
    defaultDistPref, srcPref,
    hscolourPref, haddockPref,
    autogenModulesDir,

    autogenModuleName,
    cppHeaderName,
    haddockName,

    mkLibName,
    mkProfLibName,
    mkSharedLibName,

    exeExtension,
    objExtension,
    dllExtension,

  ) where


import System.FilePath ((</>), (<.>))

import Distribution.Package
         ( PackageIdentifier, packageName )
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Compiler
         ( CompilerId(..) )
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Distribution.Simple.Setup (defaultDistPref)
import Distribution.Text
         ( display )
import Distribution.System (OS(..), buildOS)

-- ---------------------------------------------------------------------------
-- Build directories and files

srcPref :: FilePath -> FilePath
srcPref distPref = distPref </> "src"

hscolourPref :: FilePath -> PackageDescription -> FilePath
hscolourPref = haddockPref

haddockPref :: FilePath -> PackageDescription -> FilePath
haddockPref distPref pkg_descr
    = distPref </> "doc" </> "html" </> display (packageName pkg_descr)

-- |The directory in which we put auto-generated modules
autogenModulesDir :: LocalBuildInfo -> String
autogenModulesDir lbi = buildDir lbi </> "autogen"

cppHeaderName :: String
cppHeaderName = "cabal_macros.h"

-- |The name of the auto-generated module associated with a package
autogenModuleName :: PackageDescription -> ModuleName
autogenModuleName pkg_descr =
  ModuleName.fromString $
    "Paths_" ++ map fixchar (display (packageName pkg_descr))
  where fixchar '-' = '_'
        fixchar c   = c

haddockName :: PackageDescription -> FilePath
haddockName pkg_descr = display (packageName pkg_descr) <.> "haddock"

-- ---------------------------------------------------------------------------
-- Library file names

mkLibName :: PackageIdentifier -> String
mkLibName lib = "libHS" ++ display lib <.> "a"

mkProfLibName :: PackageIdentifier -> String
mkProfLibName lib =  "libHS" ++ display lib ++ "_p" <.> "a"

-- Implement proper name mangling for dynamical shared objects
-- libHS<packagename>-<compilerFlavour><compilerVersion>
-- e.g. libHSbase-2.1-ghc6.6.1.so
mkSharedLibName :: PackageIdentifier -> CompilerId -> String
mkSharedLibName lib (CompilerId compilerFlavor compilerVersion)
  = "libHS" ++ display lib ++ "-" ++ comp <.> dllExtension
  where comp = display compilerFlavor ++ display compilerVersion

-- ------------------------------------------------------------
-- * Platform file extensions
-- ------------------------------------------------------------

-- ToDo: This should be determined via autoconf (AC_EXEEXT)
-- | Extension for executable files
-- (typically @\"\"@ on Unix and @\"exe\"@ on Windows or OS\/2)
exeExtension :: String
exeExtension = case buildOS of
                   Windows -> "exe"
                   _       -> ""

-- ToDo: This should be determined via autoconf (AC_OBJEXT)
-- | Extension for object files. For GHC and NHC the extension is @\"o\"@.
-- Hugs uses either @\"o\"@ or @\"obj\"@ depending on the used C compiler.
objExtension :: String
objExtension = "o"

-- | Extension for dynamically linked (or shared) libraries
-- (typically @\"so\"@ on Unix and @\"dll\"@ on Windows)
dllExtension :: String
dllExtension = case buildOS of
                   Windows -> "dll"
                   OSX     -> "dylib"
                   _       -> "so"
