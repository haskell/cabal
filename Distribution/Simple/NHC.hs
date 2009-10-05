-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.NHC
-- Copyright   :  Isaac Jones 2003-2006
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the NHC-specific code for configuring, building
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

module Distribution.Simple.NHC
  ( configure
  , buildLib, buildExe
  , installLib, installExe
  ) where

import Distribution.Package
        ( PackageIdentifier, packageName, Package(..) )
import Distribution.PackageDescription
        ( PackageDescription(..), BuildInfo(..), Library(..), Executable(..)
        , hcOptions )
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Simple.LocalBuildInfo
        ( LocalBuildInfo(..), ComponentLocalBuildInfo(..) )
import Distribution.Simple.BuildPaths
        ( mkLibName, objExtension, exeExtension )
import Distribution.Simple.Compiler
        ( CompilerFlavor(..), CompilerId(..), Compiler(..)
        , Flag, extensionsToFlags )
import Language.Haskell.Extension
        ( Extension(..) )
import Distribution.Simple.Program
         ( ProgramConfiguration, userMaybeSpecifyPath, programPath
         , requireProgram, requireProgramVersion, lookupProgram
         , nhcProgram, hmakeProgram, ldProgram, arProgram
         , rawSystemProgramConf )
import Distribution.Simple.Utils
        ( die, info, findFileWithExtension, findModuleFiles
        , installOrdinaryFile, installExecutableFile, installOrdinaryFiles
        , createDirectoryIfMissingVerbose )
import Distribution.Version
        ( Version(..), orLaterVersion )
import Distribution.Verbosity
import Distribution.Text
        ( display )

import System.FilePath
        ( (</>), (<.>), normalise, takeDirectory, dropExtension )
import System.Directory
        ( removeFile )

import Data.List ( nub )
import Control.Monad ( when, unless )
import Distribution.Compat.Exception

-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, ProgramConfiguration)
configure verbosity hcPath _hcPkgPath conf = do

  (_nhcProg, nhcVersion, conf') <-
    requireProgramVersion verbosity nhcProgram
      (orLaterVersion (Version [1,20] []))
      (userMaybeSpecifyPath "nhc98" hcPath conf)

  (_hmakeProg, _hmakeVersion, conf'') <-
    requireProgramVersion verbosity hmakeProgram
     (orLaterVersion (Version [3,13] [])) conf'
  (_ldProg, conf''')   <- requireProgram verbosity ldProgram conf''
  (_arProg, conf'''')  <- requireProgram verbosity arProgram conf'''

  --TODO: put this stuff in a monad so we can say just:
  -- requireProgram hmakeProgram (orLaterVersion (Version [3,13] []))
  -- requireProgram ldProgram anyVersion
  -- requireProgram ldPrograrProgramam anyVersion
  -- unless (null (cSources bi)) $ requireProgram ccProgram anyVersion

  let comp = Compiler {
        compilerId         = CompilerId NHC nhcVersion,
        compilerExtensions = nhcLanguageExtensions
      }
  return (comp, conf'''')

-- | The flags for the supported extensions
nhcLanguageExtensions :: [(Extension, Flag)]
nhcLanguageExtensions =
    -- TODO: use -98 when no extensions are specified.
    -- NHC doesn't enforce the monomorphism restriction at all.
    [(NoMonomorphismRestriction, "")
    ,(ForeignFunctionInterface,  "")
    ,(ExistentialQuantification, "")
    ,(EmptyDataDecls,            "")
    ,(NamedFieldPuns,            "-puns")
    ,(CPP,                       "-cpp")
    ]

-- -----------------------------------------------------------------------------
-- Building

-- |FIX: For now, the target must contain a main module.  Not used
-- ATM. Re-add later.
buildLib :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity pkg_descr lbi lib clbi = do
  let conf = withPrograms lbi
      Just nhcProg = lookupProgram nhcProgram conf
  let bi = libBuildInfo lib
      modules = exposedModules lib ++ otherModules bi
      -- Unsupported extensions have already been checked by configure
      extensionFlags = extensionsToFlags (compiler lbi) (extensions bi)
  inFiles <- getModulePaths lbi bi modules
  let targetDir = buildDir lbi
      srcDirs  = nub (map takeDirectory inFiles)
      destDirs = map (targetDir </>) srcDirs
  mapM_ (createDirectoryIfMissingVerbose verbosity True) destDirs
  rawSystemProgramConf verbosity hmakeProgram conf $
       ["-hc=" ++ programPath nhcProg]
    ++ nhcVerbosityOptions verbosity
    ++ ["-d", targetDir, "-hidir", targetDir]
    ++ extensionFlags
    ++ maybe [] (hcOptions NHC . libBuildInfo)
                           (library pkg_descr)
    ++ concat [ ["-package", display (packageName pkgid) ]
              | (_, pkgid) <- componentPackageDeps clbi ]
    ++ inFiles
{-
  -- build any C sources
  unless (null (cSources bi)) $ do
     info verbosity "Building C Sources..."
     let commonCcArgs = (if verbosity >= deafening then ["-v"] else [])
                     ++ ["-I" ++ dir | dir <- includeDirs bi]
                     ++ [opt | opt <- ccOptions bi]
                     ++ (if withOptimization lbi then ["-O2"] else [])
     flip mapM_ (cSources bi) $ \cfile -> do
       let ofile = targetDir </> cfile `replaceExtension` objExtension
       createDirectoryIfMissingVerbose verbosity True (takeDirectory ofile)
       rawSystemProgramConf verbosity hmakeProgram conf
         (commonCcArgs ++ ["-c", cfile, "-o", ofile])
-}
  -- link:
  info verbosity "Linking..."
  let --cObjs = [ targetDir </> cFile `replaceExtension` objExtension
      --        | cFile <- cSources bi ]
      libFilePath = targetDir </> mkLibName (packageId pkg_descr)
      hObjs = [ targetDir </> ModuleName.toFilePath m <.> objExtension
              | m <- modules ]

  unless (null hObjs {-&& null cObjs-}) $ do
    -- first remove library if it exists
    removeFile libFilePath `catchIO` \_ -> return ()

    let arVerbosity | verbosity >= deafening = "v"
                    | verbosity >= normal = ""
                    | otherwise = "c"

    rawSystemProgramConf verbosity arProgram (withPrograms lbi) $
         ["q"++ arVerbosity, libFilePath]
      ++ hObjs
--    ++ cObjs

-- | Building an executable for NHC.
buildExe :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe verbosity pkg_descr lbi exe clbi = do
  let conf = withPrograms lbi
      Just nhcProg = lookupProgram nhcProgram conf
  when (dropExtension (modulePath exe) /= exeName exe) $
    die $ "hmake does not support exe names that do not match the name of "
       ++ "the 'main-is' file. You will have to rename your executable to "
       ++ show (dropExtension (modulePath exe))
  let bi = buildInfo exe
      modules = otherModules bi
      -- Unsupported extensions have already been checked by configure
      extensionFlags = extensionsToFlags (compiler lbi) (extensions bi)
  inFiles <- getModulePaths lbi bi modules
  let targetDir = buildDir lbi </> exeName exe
      exeDir    = targetDir </> (exeName exe ++ "-tmp")
      srcDirs   = nub (map takeDirectory (modulePath exe : inFiles))
      destDirs  = map (exeDir </>) srcDirs
  mapM_ (createDirectoryIfMissingVerbose verbosity True) destDirs
  rawSystemProgramConf verbosity hmakeProgram conf $
       ["-hc=" ++ programPath nhcProg]
    ++ nhcVerbosityOptions verbosity
    ++ ["-d", targetDir, "-hidir", targetDir]
    ++ extensionFlags
    ++ maybe [] (hcOptions NHC . libBuildInfo)
                           (library pkg_descr)
    ++ concat [ ["-package", display (packageName pkgid) ]
              | (_, pkgid) <- componentPackageDeps clbi ]
    ++ inFiles
    ++ [exeName exe]

nhcVerbosityOptions :: Verbosity -> [String]
nhcVerbosityOptions verbosity
     | verbosity >= deafening = ["-v"]
     | verbosity >= normal    = []
     | otherwise              = ["-q"]

--TODO: where to put this? it's duplicated in .Simple too
getModulePaths :: LocalBuildInfo -> BuildInfo -> [ModuleName] -> IO [FilePath]
getModulePaths lbi bi modules = sequence
   [ findFileWithExtension ["hs", "lhs"] (buildDir lbi : hsSourceDirs bi)
       (ModuleName.toFilePath module_) >>= maybe (notFound module_) (return . normalise)
   | module_ <- modules ]
   where notFound module_ = die $ "can't find source for module " ++ display module_

-- -----------------------------------------------------------------------------
-- Installing

-- |Install executables for NHC.
installExe :: Verbosity -- ^verbosity
           -> FilePath  -- ^install location
           -> FilePath  -- ^Build location
           -> (FilePath, FilePath)  -- ^Executable (prefix,suffix)
           -> Executable
           -> IO ()
installExe verbosity pref buildPref (progprefix,progsuffix) exe
    = do createDirectoryIfMissingVerbose verbosity True pref
         let exeBaseName = exeName exe
             exeFileName = exeBaseName <.> exeExtension
             fixedExeFileName = (progprefix ++ exeBaseName ++ progsuffix) <.> exeExtension
         installExecutableFile verbosity
           (buildPref </> exeBaseName </> exeFileName)
           (pref </> fixedExeFileName)

-- |Install for nhc98: .hi and .a files
installLib    :: Verbosity -- ^verbosity
              -> FilePath  -- ^install location
              -> FilePath  -- ^Build location
              -> PackageIdentifier
              -> Library
              -> IO ()
installLib verbosity pref buildPref pkgid lib
    = do let bi = libBuildInfo lib
             modules = exposedModules lib ++ otherModules bi
         findModuleFiles [buildPref] ["hi"] modules
           >>= installOrdinaryFiles verbosity pref
         let libName = mkLibName pkgid
         installOrdinaryFile verbosity (buildPref </> libName) (pref </> libName)
