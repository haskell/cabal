-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.UHC
-- Copyright   :  Andres Loeh 2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the UHC-specific code for configuring, building
-- and installing packages.

{-
Copyright (c) 2009, Andres Loeh
Copyright (c) 2003-2005, Isaac Jones
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

module Distribution.Simple.UHC (
    configure, buildLib
  ) where

import Data.List
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler as C
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Text
import Distribution.Verbosity
import Distribution.Version
import Language.Haskell.Extension
import System.FilePath

-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, ProgramConfiguration)
configure verbosity hcPath _hcPkgPath conf = do

  (_uhcProg, uhcVersion, conf') <-
    requireProgramVersion verbosity uhcProgram
    (orLaterVersion (Version [1,0,2] []))
    (userMaybeSpecifyPath "uhc" hcPath conf)

  let comp = Compiler {
               compilerId          =  CompilerId UHC uhcVersion,
               compilerExtensions  =  uhcLanguageExtensions
             }
  return (comp, conf')

-- | The flags for the supported extensions.
uhcLanguageExtensions :: [(Extension, C.Flag)]
uhcLanguageExtensions = []


-- -----------------------------------------------------------------------------
-- Building

buildLib :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity pkg_descr lbi lib clbi = do

  let runUhcProg = rawSystemProgramConf verbosity uhcProgram (withPrograms lbi)
  let uhcArgs =    -- set package name
                   ["--pkg-build=" ++ display (packageId pkg_descr)]
                   -- common flags lib/exe
                ++ constructUHCCmdLine lbi (libBuildInfo lib) clbi
                                       (buildDir lbi) verbosity
                   -- source files
                   -- suboptimal: UHC does not understand module names, so
                   -- we replace periods by path separators
                ++ map (map (\ c -> if c == '.' then pathSeparator else c))
                       (map display (libModules lib))

  runUhcProg uhcArgs
  
  return ()

constructUHCCmdLine :: LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo
                    -> FilePath -> Verbosity -> [String]
constructUHCCmdLine lbi bi clbi odir verbosity =
     -- verbosity
     (if      verbosity >= deafening then ["-v4"]
      else if verbosity >= normal    then []
      else                                ["-v0"])
     -- packages
  ++ ["--hide-all-packages"]
  ++ ["--package=" ++ display (pkgName pkgid) | (_, pkgid) <- componentPackageDeps clbi ]
     -- search paths
  ++ ["-i" ++ odir]
  ++ ["-i" ++ l | l <- nub (hsSourceDirs bi)]
  ++ ["-i" ++ autogenModulesDir lbi]
     -- output path
  ++ ["--odir=" ++ odir]
     -- optimization
  ++ (case withOptimization lbi of
        NoOptimisation       ->  ["-O0"]
        NormalOptimisation   ->  ["-O1"]
        MaximumOptimisation  ->  ["-O2"])
