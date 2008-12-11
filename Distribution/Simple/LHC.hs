-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.LHC
-- Copyright   :  Isaac Jones 2003-2006
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the LHC-specific code for configuring, building
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

module Distribution.Simple.LHC (
        configure, getInstalledPackages, build, installLib, installExe,
        getLhcLibDirsFromVersion
 ) where

import Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(..), hcOptions
         , Executable(..), withExe, Library(..), withLib )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..) )
import Distribution.Simple.BuildPaths
         ( autogenModulesDir, exeExtension )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), CompilerId(..), Compiler(..)
         , PackageDB(..), PackageDBStack, Flag, extensionsToFlags )
import Language.Haskell.Extension (Extension(..))
import Distribution.Simple.Program
         ( ConfiguredProgram(..), lhcProgram, ProgramConfiguration
         , userMaybeSpecifyPath, requireProgram, lookupProgram
         , userSpecifyArgs, rawSystemProgram )
import Distribution.Version
         ( Version(..), anyVersion )
import Distribution.Package
         ( Package(..), packageName, packageVersion )
import Distribution.Simple.Utils
        ( createDirectoryIfMissingVerbose, copyFileVerbose, writeFileAtomic
        , info, intercalate )
import Distribution.Verbosity
import Distribution.Text
         ( display )

import System.FilePath          ( (</>) )
import System.Directory         ( getAppUserDataDirectory )
import Data.List                ( nub )
import Data.Maybe               ( catMaybes )

import qualified Distribution.Simple.GHC as GHC


-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, ProgramConfiguration)
configure verbosity hcPath _hcPkgPath conf = do

  (lhcProg, conf')  <- requireProgram verbosity lhcProgram anyVersion
                         (userMaybeSpecifyPath "lhc" hcPath conf)

  let Just (Version digits _)  = programVersion lhcProg
      -- only the first three version digits are significant
      version                  = Version (take 3 digits) []
      comp = Compiler {
        compilerId             = CompilerId LHC version,
        compilerExtensions     = lhcLanguageExtensions
      }
  return (comp, conf')

-- | The flags for the supported extensions
lhcLanguageExtensions :: [(Extension, Flag)]
lhcLanguageExtensions =
    [(TypeSynonymInstances       , "")
    ,(ForeignFunctionInterface   , "")
    ,(NoImplicitPrelude          , "--noprelude")
    ,(CPP                        , "-fcpp")
    ]

getLhcLibDirs :: Verbosity -> ProgramConfiguration -> IO (String, String)
getLhcLibDirs verbosity conf
    = do (lhc,_conf') <- requireProgram verbosity lhcProgram anyVersion conf
         getLhcLibDirsFromVersion (programVersion lhc)

getLhcLibDirsFromVersion :: Maybe Version -> IO (String, String)
getLhcLibDirsFromVersion (Just (Version (x:y:_) tags))
    = do let v = Version [x,y] tags
         app <- getAppUserDataDirectory "lhc"
         return ("/usr/lib/lhc-"++display v,app ++ "/"++display v)
getLhcLibDirsFromVersion _
    = return ("","")


getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramConfiguration
                    -> IO (PackageIndex InstalledPackageInfo)
getInstalledPackages verbosity packageDBs conf = do
   (globalDir, userDir) <- getLhcLibDirs verbosity conf
   let (extraArgs, packageDBs') = (\(a,b) -> (catMaybes a, catMaybes b))
                                . unzip
                                . map (fixPackageDB globalDir userDir)
                                $ packageDBs

   -- Yes, LHC really does use ghc-pkg (with a different package.conf).
   GHC.getInstalledPackages verbosity packageDBs' $
     userSpecifyArgs "ghc-pkg" extraArgs conf

  where
    fixPackageDB globalDir userDir packageDB = case packageDB of
      GlobalPackageDB     -> (Just flag, Just packageDB)
                               where flag = "--global-conf="
                                         ++ globalDir </> "package.conf"
      UserPackageDB       -> (Nothing, Just packageDB')
                               where packageDB' = SpecificPackageDB
                                                    (userDir </> "package.conf")
      SpecificPackageDB _ -> (Nothing, Just packageDB)

-- -----------------------------------------------------------------------------
-- Building

-- | Building a package for LHC.
-- Currently C source files are not supported.
build :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
build pkg_descr lbi verbosity = do
  let Just lhcProg = lookupProgram lhcProgram (withPrograms lbi)
  withLib pkg_descr () $ \lib -> do
      info verbosity "Building library..."
      let libBi = libBuildInfo lib
      let args  = constructLHCCmdLine lbi libBi (buildDir lbi) verbosity
      let pkgid = display (packageId pkg_descr)
          pfile = buildDir lbi </> "lhc-pkg.conf"
          hlfile= buildDir lbi </> (pkgid ++ ".hl")
      writeFileAtomic pfile $ lhcPkgConf pkg_descr
      rawSystemProgram verbosity lhcProg $ args ++ ["-c","--build-hl="++pfile, "-o", hlfile]
  withExe pkg_descr $ \exe -> do
      info verbosity ("Building executable "++exeName exe)
      let exeBi = buildInfo exe
      let out   = buildDir lbi </> exeName exe
      let args  = constructLHCCmdLine lbi exeBi (buildDir lbi) verbosity
      rawSystemProgram verbosity lhcProg (["-o",out] ++ args ++ [modulePath exe])

constructLHCCmdLine :: LocalBuildInfo -> BuildInfo -> FilePath -> Verbosity -> [String]
constructLHCCmdLine lbi bi _odir verbosity =
        (if verbosity >= deafening then ["-v"] else [])
     ++ extensionsToFlags (compiler lbi) (extensions bi)
     ++ hcOptions LHC bi
     ++ ["--noauto","-i-"]
     ++ ["--ho-dir",buildDir lbi]
     ++ concat [["-i", l] | l <- nub (hsSourceDirs bi)]
     ++ ["-i", autogenModulesDir lbi]
     ++ ["-optc" ++ opt | opt <- PD.ccOptions bi]
     ++ (concat [ ["-p", display pkg] | pkg <- packageDeps lbi ])

lhcPkgConf :: PackageDescription -> String
lhcPkgConf pd =
  let sline name sel = name ++ ": "++sel pd
      Just lib = library pd
      comma = intercalate "," . map display
  in unlines [sline "name" (display . packageName)
             ,sline "version" (display . packageVersion)
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

