-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.JHC
-- Copyright   :  Isaac Jones 2003-2006
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the JHC-specific code for configuring, building
-- and installing packages.

module Distribution.Simple.JHC (
        configure, getInstalledPackages,
        buildLib, buildExe,
        installLib, installExe
 ) where

import Distribution.PackageDescription as PD
       ( PackageDescription(..), BuildInfo(..), Executable(..)
       , Library(..), libModules, hcOptions, usedExtensions )
import Distribution.InstalledPackageInfo
         ( emptyInstalledPackageInfo, )
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), ComponentLocalBuildInfo(..) )
import Distribution.Simple.BuildPaths
                                ( autogenModulesDir, exeExtension )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), CompilerId(..), Compiler(..), AbiTag(..)
         , PackageDBStack, Flag, languageToFlags, extensionsToFlags )
import Language.Haskell.Extension
         ( Language(Haskell98), Extension(..), KnownExtension(..))
import Distribution.Simple.Program
         ( ConfiguredProgram(..), jhcProgram, ProgramConfiguration
         , userMaybeSpecifyPath, requireProgramVersion, lookupProgram
         , rawSystemProgram, rawSystemProgramStdoutConf )
import Distribution.Version
         ( Version(..), orLaterVersion )
import Distribution.Package
         ( Package(..), InstalledPackageId(InstalledPackageId),
           pkgName, pkgVersion, )
import Distribution.Simple.Utils
        ( createDirectoryIfMissingVerbose, writeFileAtomic
        , installOrdinaryFile, installExecutableFile
        , intercalate )
import System.FilePath          ( (</>) )
import Distribution.Verbosity
import Distribution.Text
         ( Text(parse), display )
import Distribution.Compat.ReadP
    ( readP_to_S, string, skipSpaces )
import Distribution.System ( Platform )

import Data.List                ( nub )
import Data.Char                ( isSpace )
import qualified Data.Map as M  ( empty )
import Data.Maybe               ( fromMaybe )

import qualified Data.ByteString.Lazy.Char8 as BS.Char8


-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, Maybe Platform, ProgramConfiguration)
configure verbosity hcPath _hcPkgPath conf = do

  (jhcProg, _, conf') <- requireProgramVersion verbosity
                           jhcProgram (orLaterVersion (Version [0,7,2] []))
                           (userMaybeSpecifyPath "jhc" hcPath conf)

  let Just version = programVersion jhcProg
      comp = Compiler {
        compilerId             = CompilerId JHC version,
        compilerAbiTag         = NoAbiTag,
        compilerCompat         = [],
        compilerLanguages      = jhcLanguages,
        compilerExtensions     = jhcLanguageExtensions,
        compilerProperties     = M.empty
      }
      compPlatform = Nothing
  return (comp, compPlatform, conf')

jhcLanguages :: [(Language, Flag)]
jhcLanguages = [(Haskell98, "")]

-- | The flags for the supported extensions
jhcLanguageExtensions :: [(Extension, Flag)]
jhcLanguageExtensions =
    [(EnableExtension  TypeSynonymInstances       , "")
    ,(DisableExtension TypeSynonymInstances       , "")
    ,(EnableExtension  ForeignFunctionInterface   , "")
    ,(DisableExtension ForeignFunctionInterface   , "")
    ,(EnableExtension  ImplicitPrelude            , "") -- Wrong
    ,(DisableExtension ImplicitPrelude            , "--noprelude")
    ,(EnableExtension  CPP                        , "-fcpp")
    ,(DisableExtension CPP                        , "-fno-cpp")
    ]

getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramConfiguration
                    -> IO InstalledPackageIndex
getInstalledPackages verbosity _packageDBs conf = do
   -- jhc --list-libraries lists all available libraries.
   -- How shall I find out, whether they are global or local
   -- without checking all files and locations?
   str <- rawSystemProgramStdoutConf verbosity jhcProgram conf ["--list-libraries"]
   let pCheck :: [(a, String)] -> [a]
       pCheck rs = [ r | (r,s) <- rs, all isSpace s ]
   let parseLine ln =
          pCheck (readP_to_S
             (skipSpaces >> string "Name:" >> skipSpaces >> parse) ln)
   return $
      PackageIndex.fromList $
      map (\p -> emptyInstalledPackageInfo {
                    InstalledPackageInfo.installedPackageId =
                       InstalledPackageId (display p),
                    InstalledPackageInfo.sourcePackageId = p
                 }) $
      concatMap parseLine $
      lines str

-- -----------------------------------------------------------------------------
-- Building

-- | Building a package for JHC.
-- Currently C source files are not supported.
buildLib :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity pkg_descr lbi lib clbi = do
  let Just jhcProg = lookupProgram jhcProgram (withPrograms lbi)
  let libBi = libBuildInfo lib
  let args  = constructJHCCmdLine lbi libBi clbi (buildDir lbi) verbosity
  let pkgid = display (packageId pkg_descr)
      pfile = buildDir lbi </> "jhc-pkg.conf"
      hlfile= buildDir lbi </> (pkgid ++ ".hl")
  writeFileAtomic pfile . BS.Char8.pack $ jhcPkgConf pkg_descr
  rawSystemProgram verbosity jhcProg $
     ["--build-hl="++pfile, "-o", hlfile] ++
     args ++ map display (libModules lib)

-- | Building an executable for JHC.
-- Currently C source files are not supported.
buildExe :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe verbosity _pkg_descr lbi exe clbi = do
  let Just jhcProg = lookupProgram jhcProgram (withPrograms lbi)
  let exeBi = buildInfo exe
  let out   = buildDir lbi </> exeName exe
  let args  = constructJHCCmdLine lbi exeBi clbi (buildDir lbi) verbosity
  rawSystemProgram verbosity jhcProg (["-o",out] ++ args ++ [modulePath exe])

constructJHCCmdLine :: LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo
                    -> FilePath -> Verbosity -> [String]
constructJHCCmdLine lbi bi clbi _odir verbosity =
        (if verbosity >= deafening then ["-v"] else [])
     ++ hcOptions JHC bi
     ++ languageToFlags (compiler lbi) (defaultLanguage bi)
     ++ extensionsToFlags (compiler lbi) (usedExtensions bi)
     ++ ["--noauto","-i-"]
     ++ concat [["-i", l] | l <- nub (hsSourceDirs bi)]
     ++ ["-i", autogenModulesDir lbi]
     ++ ["-optc" ++ opt | opt <- PD.ccOptions bi]
     -- It would be better if JHC would accept package names with versions,
     -- but JHC-0.7.2 doesn't accept this.
     -- Thus, we have to strip the version with 'pkgName'.
     ++ (concat [ ["-p", display (pkgName pkgid)]
                | (_, pkgid) <- componentPackageDeps clbi ])

jhcPkgConf :: PackageDescription -> String
jhcPkgConf pd =
  let sline name sel = name ++ ": "++sel pd
      lib = fromMaybe (error "no library available") . library
      comma = intercalate "," . map display
  in unlines [sline "name" (display . pkgName . packageId)
             ,sline "version" (display . pkgVersion . packageId)
             ,sline "exposed-modules" (comma . PD.exposedModules . lib)
             ,sline "hidden-modules" (comma . otherModules . libBuildInfo . lib)
             ]

installLib :: Verbosity -> FilePath -> FilePath -> PackageDescription -> Library -> IO ()
installLib verb dest build_dir pkg_descr _ = do
    let p = display (packageId pkg_descr)++".hl"
    createDirectoryIfMissingVerbose verb True dest
    installOrdinaryFile verb (build_dir </> p) (dest </> p)

installExe :: Verbosity -> FilePath -> FilePath -> (FilePath,FilePath) -> PackageDescription -> Executable -> IO ()
installExe verb dest build_dir (progprefix,progsuffix) _ exe = do
    let exe_name = exeName exe
        src = exe_name </> exeExtension
        out   = (progprefix ++ exe_name ++ progsuffix) </> exeExtension
    createDirectoryIfMissingVerbose verb True dest
    installExecutableFile verb (build_dir </> src) (dest </> out)
