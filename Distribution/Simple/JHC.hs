-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.JHC
-- Copyright   :  Isaac Jones 2003-2006
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the JHC-specific code for configuring, building
-- and installing packages.

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

module Distribution.Simple.JHC (
        configure, getInstalledPackages, build, installLib, installExe
 ) where

import Distribution.PackageDescription as PD
                                ( PackageDescription(..), BuildInfo(..),
                                  withLib,
                                  Executable(..), withExe, Library(..),
                                  libModules, hcOptions )
import Distribution.InstalledPackageInfo
                                ( InstalledPackageInfo, emptyInstalledPackageInfo )
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
                                ( InstalledPackageInfo_(package) )
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.LocalBuildInfo
                                ( LocalBuildInfo(..) )
import Distribution.Simple.BuildPaths
                                ( autogenModulesDir, exeExtension )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), CompilerId(..), Compiler(..)
         , PackageDB, Flag, extensionsToFlags )
import Language.Haskell.Extension (Extension(..))
import Distribution.Simple.Program     ( ConfiguredProgram(..), jhcProgram,
                                  ProgramConfiguration, userMaybeSpecifyPath,
                                  requireProgram, lookupProgram,
                                  rawSystemProgram, rawSystemProgramStdoutConf )
import Distribution.Version     ( VersionRange(AnyVersion) )
import Distribution.Package
         ( Package(..) )
import Distribution.Simple.Utils
        ( createDirectoryIfMissingVerbose, copyFileVerbose, writeFileAtomic
        , die, info, intercalate )
import System.FilePath          ( (</>) )
import Distribution.Verbosity
import Distribution.Text
         ( Text(parse), display )
import Distribution.Compat.ReadP
    ( readP_to_S, many, skipSpaces )

import Data.List                ( nub )
import Data.Char                ( isSpace )



-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, ProgramConfiguration)
configure verbosity hcPath _hcPkgPath conf = do

  (jhcProg, conf')  <- requireProgram verbosity jhcProgram AnyVersion
                         (userMaybeSpecifyPath "jhc" hcPath conf)

  let Just version = programVersion jhcProg
      comp = Compiler {
        compilerId             = CompilerId JHC version,
        compilerExtensions     = jhcLanguageExtensions
      }
  return (comp, conf')

-- | The flags for the supported extensions
jhcLanguageExtensions :: [(Extension, Flag)]
jhcLanguageExtensions =
    [(TypeSynonymInstances       , "")
    ,(ForeignFunctionInterface   , "")
    ,(NoImplicitPrelude          , "--noprelude")
    ,(CPP                        , "-fcpp")
    ]

getInstalledPackages :: Verbosity -> PackageDB -> ProgramConfiguration
                    -> IO (PackageIndex InstalledPackageInfo)
getInstalledPackages verbosity _packagedb conf = do
   str <- rawSystemProgramStdoutConf verbosity jhcProgram conf ["--list-libraries"]
   case pCheck (readP_to_S (many (skipSpaces >> parse)) str) of
     [ps] -> return $ PackageIndex.fromList
                    [ emptyInstalledPackageInfo {
                        InstalledPackageInfo.package = p
                      }
                    | p <- ps ]
     _    -> die "cannot parse package list"
  where
    pCheck :: [(a, [Char])] -> [a]
    pCheck rs = [ r | (r,s) <- rs, all isSpace s ]

-- -----------------------------------------------------------------------------
-- Building

-- | Building a package for JHC.
-- Currently C source files are not supported.
build :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
build pkg_descr lbi verbosity = do
  let Just jhcProg = lookupProgram jhcProgram (withPrograms lbi)
  withLib pkg_descr () $ \lib -> do
      info verbosity "Building library..."
      let libBi = libBuildInfo lib
      let args  = constructJHCCmdLine lbi libBi (buildDir lbi) verbosity
      rawSystemProgram verbosity jhcProg $
        ["-c"] ++ args ++ map display (libModules pkg_descr)
      let pkgid = display (packageId pkg_descr)
          pfile = buildDir lbi </> "jhc-pkg.conf"
          hlfile= buildDir lbi </> (pkgid ++ ".hl")
      writeFileAtomic pfile $ jhcPkgConf pkg_descr
      rawSystemProgram verbosity jhcProg ["--build-hl="++pfile, "-o", hlfile]
  withExe pkg_descr $ \exe -> do
      info verbosity ("Building executable "++exeName exe)
      let exeBi = buildInfo exe
      let out   = buildDir lbi </> exeName exe
      let args  = constructJHCCmdLine lbi exeBi (buildDir lbi) verbosity
      rawSystemProgram verbosity jhcProg (["-o",out] ++ args ++ [modulePath exe])

constructJHCCmdLine :: LocalBuildInfo -> BuildInfo -> FilePath -> Verbosity -> [String]
constructJHCCmdLine lbi bi _odir verbosity =
        (if verbosity >= deafening then ["-v"] else [])
     ++ extensionsToFlags (compiler lbi) (extensions bi)
     ++ hcOptions JHC bi
     ++ ["--noauto","-i-"]
     ++ concat [["-i", l] | l <- nub (hsSourceDirs bi)]
     ++ ["-i", autogenModulesDir lbi]
     ++ ["-optc" ++ opt | opt <- PD.ccOptions bi]
     ++ (concat [ ["-p", display pkg] | pkg <- packageDeps lbi ])

jhcPkgConf :: PackageDescription -> String
jhcPkgConf pd =
  let sline name sel = name ++ ": "++sel pd
      Just lib = library pd
      comma = intercalate "," . map display
  in unlines [sline "name" (display . packageId)
             ,"exposed-modules: " ++ (comma (PD.exposedModules lib))
             ,"hidden-modules: " ++ (comma (otherModules $ libBuildInfo lib))
             ]

installLib :: Verbosity -> FilePath -> FilePath -> PackageDescription -> Library -> IO ()
installLib verb dest build_dir pkg_descr _ = do
    let p = display (packageId pkg_descr)++".hl"
    createDirectoryIfMissingVerbose verb True dest
    copyFileVerbose verb (build_dir </> p) (dest </> p)

installExe :: Verbosity -> FilePath -> FilePath -> (FilePath,FilePath) -> PackageDescription -> Executable -> IO ()
installExe verb dest build_dir (progprefix,progsuffix) _ exe = do
    let exe_name = exeName exe
        src = exe_name </> exeExtension
        out   = (progprefix ++ exe_name ++ progsuffix) </> exeExtension
    createDirectoryIfMissingVerbose verb True dest
    copyFileVerbose verb (build_dir </> src) (dest </> out)

