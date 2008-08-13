-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build
-- Copyright   :  Isaac Jones 2003-2005,
--                Ross Paterson 2006,
--                Duncan Coutts 2007-2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point to actually building the modules in a package. It
-- doesn't actually do much itself, most of the work is delegated to
-- compiler-specific actions. It does do some non-compiler specific bits like
-- running pre-processors.
--
-- There's some stuff to do with generating @makefiles@ which is a well hidden
-- feature that's used to build libraries inside the GHC build system but which
-- we'd like to kill off and replace with something better (doing our own
-- dependency analysis properly).
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

module Distribution.Simple.Build (
    build,
    makefile,

    initialBuildSteps,
    writeAutogenFiles,
  ) where

import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.JHC  as JHC
import qualified Distribution.Simple.NHC  as NHC
import qualified Distribution.Simple.Hugs as Hugs

import qualified Distribution.Simple.Build.Macros      as Build.Macros
import qualified Distribution.Simple.Build.PathsModule as Build.PathsModule

import Distribution.Package
         ( Package(..) )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), compilerFlavor )
import Distribution.PackageDescription
         ( PackageDescription(..), BuildInfo(..)
         , Executable(..), Library(..), hasLibs )
import qualified Distribution.ModuleName as ModuleName

import Distribution.Simple.Setup
         ( BuildFlags(..), MakefileFlags(..), fromFlag )
import Distribution.Simple.PreProcess
         ( preprocessSources, PPSuffixHandler )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(compiler, buildDir) )
import Distribution.Simple.BuildPaths
         ( autogenModulesDir, autogenModuleName, cppHeaderName )
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, die, setupMessage, rewriteFile )

import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text
         ( display )

import Data.Maybe
         ( maybeToList )
import Control.Monad
         ( unless, when )
import System.FilePath
         ( (</>), (<.>) )

-- -----------------------------------------------------------------------------
-- |Build the libraries and executables in this package.

build    :: PackageDescription  -- ^mostly information from the .cabal file
         -> LocalBuildInfo -- ^Configuration information
         -> BuildFlags -- ^Flags that the user passed to build
         -> [ PPSuffixHandler ] -- ^preprocessors to run before compiling
         -> IO ()
build pkg_descr lbi flags suffixes = do
  let distPref  = fromFlag (buildDistPref flags)
      verbosity = fromFlag (buildVerbosity flags)
  initialBuildSteps distPref pkg_descr lbi verbosity suffixes
  setupMessage verbosity "Building" (packageId pkg_descr)
  case compilerFlavor (compiler lbi) of
    GHC  -> GHC.build  pkg_descr lbi verbosity
    JHC  -> JHC.build  pkg_descr lbi verbosity
    Hugs -> Hugs.build pkg_descr lbi verbosity
    NHC  -> NHC.build  pkg_descr lbi verbosity
    _    -> die ("Building is not supported with this compiler.")

makefile :: PackageDescription  -- ^mostly information from the .cabal file
         -> LocalBuildInfo -- ^Configuration information
         -> MakefileFlags -- ^Flags that the user passed to makefile
         -> [ PPSuffixHandler ] -- ^preprocessors to run before compiling
         -> IO ()
makefile pkg_descr lbi flags suffixes = do
  let distPref  = fromFlag (makefileDistPref flags)
      verbosity = fromFlag (makefileVerbosity flags)
  initialBuildSteps distPref pkg_descr lbi verbosity suffixes
  when (not (hasLibs pkg_descr)) $
      die ("Makefile is only supported for libraries, currently.")
  setupMessage verbosity "Generating Makefile" (packageId pkg_descr)
  case compilerFlavor (compiler lbi) of
    GHC  -> GHC.makefile  pkg_descr lbi flags
    _    -> die ("Generating a Makefile is not supported for this compiler.")


initialBuildSteps :: FilePath -- ^"dist" prefix
                  -> PackageDescription  -- ^mostly information from the .cabal file
                  -> LocalBuildInfo -- ^Configuration information
                  -> Verbosity -- ^The verbosity to use
                  -> [ PPSuffixHandler ] -- ^preprocessors to run before compiling
                  -> IO ()
initialBuildSteps _distPref pkg_descr lbi verbosity suffixes = do
  -- check that there's something to build
  let buildInfos =
          map libBuildInfo (maybeToList (library pkg_descr)) ++
          map buildInfo (executables pkg_descr)
  unless (any buildable buildInfos) $ do
    let name = display (packageId pkg_descr)
    die ("Package " ++ name ++ " can't be built on this system.")

  createDirectoryIfMissingVerbose verbosity True (buildDir lbi)

  writeAutogenFiles verbosity pkg_descr lbi

  preprocessSources pkg_descr lbi False verbosity suffixes

-- | Generate and write out the Paths_<pkg>.hs and cabal_macros.h files
--
writeAutogenFiles :: Verbosity
                  -> PackageDescription
                  -> LocalBuildInfo
                  -> IO ()
writeAutogenFiles verbosity pkg lbi = do
  createDirectoryIfMissingVerbose verbosity True (autogenModulesDir lbi)

  let pathsModulePath = autogenModulesDir lbi
                    </> ModuleName.toFilePath (autogenModuleName pkg) <.> "hs"
  rewriteFile pathsModulePath (Build.PathsModule.generate pkg lbi)

  let cppHeaderPath = autogenModulesDir lbi </> cppHeaderName
  rewriteFile cppHeaderPath (Build.Macros.generate pkg lbi)
