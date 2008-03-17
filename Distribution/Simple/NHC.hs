-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.NHC
-- Copyright   :  Isaac Jones 2003-2006
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
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

module Distribution.Simple.NHC
  ( configure
  , build
  , installLib, installExe
  ) where

import Distribution.Package
        ( PackageIdentifier, packageName, Package(..) )
import Distribution.PackageDescription
        ( PackageDescription(..), BuildInfo(..), Library(..), Executable(..),
          withLib, withExe, hcOptions )
import Distribution.Simple.LocalBuildInfo
        ( LocalBuildInfo(..) )
import Distribution.Simple.BuildPaths
        ( mkLibName, objExtension, exeExtension )
import Distribution.Simple.Compiler
        ( CompilerFlavor(..), CompilerId(..), Compiler(..)
        , Flag, extensionsToFlags )
import Language.Haskell.Extension
        ( Extension(..) )
import Distribution.Simple.Program 
        ( ProgramConfiguration, userMaybeSpecifyPath, requireProgram,
          lookupProgram, ConfiguredProgram(programVersion), programPath,
          nhcProgram, hmakeProgram, ldProgram, arProgram,
          rawSystemProgramConf )
import Distribution.Simple.Utils
        ( die, info, findFileWithExtension, dotToSep,
          createDirectoryIfMissingVerbose, copyFileVerbose, smartCopySources )
import Distribution.Version
        ( Version(..), VersionRange(..), orLaterVersion )
import Distribution.Verbosity
import System.FilePath
        ( (</>), (<.>), normalise, takeDirectory, dropExtension )
import System.Directory
        ( removeFile )

import Control.Exception (try)
import Data.List ( nub )
import Control.Monad ( when, unless )

-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, ProgramConfiguration)
configure verbosity hcPath _hcPkgPath conf = do

  (nhcProg, conf') <- requireProgram verbosity nhcProgram
                          (orLaterVersion (Version [1,20] []))
                          (userMaybeSpecifyPath "nhc98" hcPath conf)
  let Just nhcVersion = programVersion nhcProg

  (_hmakeProg, conf'') <- requireProgram verbosity hmakeProgram
                          (orLaterVersion (Version [3,13] [])) conf'
  (_ldProg, conf''')   <- requireProgram verbosity ldProgram AnyVersion conf''
  (_arProg, conf'''')  <- requireProgram verbosity arProgram AnyVersion conf'''

  --TODO: put this stuff in a monad so we can say just:
  -- requireProgram hmakeProgram (orLaterVersion (Version [3,13] []))
  -- requireProgram ldProgram AnyVersion
  -- requireProgram ldPrograrProgramam AnyVersion
  -- unless (null (cSources bi)) $ requireProgram ccProgram AnyVersion

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
build :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
build pkg_descr lbi verbosity = do
  let conf = withPrograms lbi
      Just nhcProg = lookupProgram nhcProgram conf
  withLib pkg_descr () $ \lib -> do
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
      ++ concat [ ["-package", packageName pkg] | pkg <- packageDeps lbi ]
      ++ inFiles
{-
    -- build any C sources
    unless (null (cSources bi)) $ do
       info verbosity "Building C Sources..."
       let commonCcArgs = (if verbosity > deafening then ["-v"] else [])
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
        hObjs = [ targetDir </> dotToSep m <.> objExtension
                | m <- modules ]

    unless (null hObjs {-&& null cObjs-}) $ do
      try (removeFile libFilePath) -- first remove library if it exists

      let arVerbosity | verbosity >= deafening = "v"
                      | verbosity >= normal = ""
                      | otherwise = "c"

      rawSystemProgramConf verbosity arProgram (withPrograms lbi) $
           ["q"++ arVerbosity, libFilePath]
        ++ hObjs
--        ++ cObjs

  withExe pkg_descr $ \exe -> do
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
      ++ concat [ ["-package", packageName pkg] | pkg <- packageDeps lbi ]
      ++ inFiles
      ++ [exeName exe]

nhcVerbosityOptions :: Verbosity -> [String]
nhcVerbosityOptions verbosity
     | verbosity >= deafening = ["-v"]
     | verbosity >= normal    = []
     | otherwise              = ["-q"]

--TODO: where to put this? it's duplicated in .Simple too
getModulePaths :: LocalBuildInfo -> BuildInfo -> [String] -> IO [FilePath]
getModulePaths lbi bi modules = sequence
   [ findFileWithExtension ["hs", "lhs"] (buildDir lbi : hsSourceDirs bi)
       (dotToSep module_) >>= maybe (notFound module_) (return . normalise)
   | module_ <- modules ]
   where notFound module_ = die $ "can't find source for module " ++ module_

-- -----------------------------------------------------------------------------
-- Installing

-- |Install executables for GHC.
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
         copyFileVerbose verbosity (buildPref </> exeBaseName </> exeFileName)
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
         smartCopySources verbosity [buildPref] pref modules ["hi"]
         let libName = mkLibName pkgid
         copyFileVerbose verbosity (buildPref </> libName) (pref </> libName)
