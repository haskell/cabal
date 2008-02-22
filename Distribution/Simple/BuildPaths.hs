-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.BuildPaths
-- Copyright   :  Isaac Jones 2003-2004,
--                Duncan Coutts 2008
--
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
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
    distPref, srcPref,
    hscolourPref, haddockPref,
    autogenModulesDir,

    autogenModuleName,
    haddockName,

    mkLibName,
    mkProfLibName,
    mkSharedLibName,

    exeExtension,
    objExtension,
    dllExtension,

  ) where


import System.FilePath (FilePath, (</>), (<.>))

import Distribution.Package (PackageIdentifier(..), Package(..))
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Distribution.Version (showVersion)
import Distribution.System (OS(..), os)

-- ---------------------------------------------------------------------------
-- Build directories and files

distPref :: FilePath
distPref = "dist"

srcPref :: FilePath
srcPref = distPref </> "src"

hscolourPref :: PackageDescription -> FilePath
hscolourPref = haddockPref

haddockPref :: PackageDescription -> FilePath
haddockPref pkg_descr
    = foldl1 (</>) [distPref, "doc", "html", pkgName (packageId pkg_descr)]

-- |The directory in which we put auto-generated modules
autogenModulesDir :: LocalBuildInfo -> String
autogenModulesDir lbi = buildDir lbi </> "autogen"


-- |The name of the auto-generated module associated with a package
autogenModuleName :: PackageDescription -> String
autogenModuleName pkg_descr =
    "Paths_" ++ map fixchar (pkgName (packageId pkg_descr))
  where fixchar '-' = '_'
        fixchar c   = c

haddockName :: PackageDescription -> FilePath
haddockName pkg_descr = pkgName (packageId pkg_descr) <.> "haddock"

-- ---------------------------------------------------------------------------
-- Library file names

mkLibName :: FilePath -- ^file Prefix
          -> String   -- ^library name.
          -> String
mkLibName pref lib = pref </> ("libHS" ++ lib ++ ".a")

mkProfLibName :: FilePath -- ^file Prefix
              -> String   -- ^library name.
              -> String
mkProfLibName pref lib = mkLibName pref (lib++"_p")

-- Implement proper name mangling for dynamical shared objects
-- libHS<packagename>-<compilerFlavour><compilerVersion>
-- e.g. libHSbase-2.1-ghc6.6.1.so
mkSharedLibName :: FilePath        -- ^file Prefix
              -> String            -- ^library name.
              -> PackageIdentifier -- ^package identifier of the compiler
              -> String
mkSharedLibName pref lib (PackageIdentifier compilerName compilerVersion)
  = pref </> ("libHS" ++ lib ++ "-" ++ comp) <.> dllExtension
  where comp = compilerName ++ showVersion compilerVersion

-- ------------------------------------------------------------
-- * Platform file extensions
-- ------------------------------------------------------------ 

-- ToDo: This should be determined via autoconf (AC_EXEEXT)
-- | Extension for executable files
-- (typically @\"\"@ on Unix and @\"exe\"@ on Windows or OS\/2)
exeExtension :: String
exeExtension = case os of
                   Windows _ -> "exe"
                   _         -> ""

-- ToDo: This should be determined via autoconf (AC_OBJEXT)
-- | Extension for object files. For GHC and NHC the extension is @\"o\"@.
-- Hugs uses either @\"o\"@ or @\"obj\"@ depending on the used C compiler.
objExtension :: String
objExtension = "o"

-- | Extension for dynamically linked (or shared) libraries
-- (typically @\"so\"@ on Unix and @\"dll\"@ on Windows)
dllExtension :: String
dllExtension = case os of
                   Windows _ -> "dll"
                   OSX       -> "dylib"
                   _         -> "so"
