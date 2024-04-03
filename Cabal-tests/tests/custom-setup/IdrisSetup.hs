-- This is Setup.hs script from idris-1.1.1

{-

Copyright (c) 2011 Edwin Brady
    School of Computer Science, University of St Andrews
All rights reserved.

This code is derived from software written by Edwin Brady
(eb@cs.st-andrews.ac.uk).

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. None of the names of the copyright holders may be used to endorse
   or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*** End of disclaimer. ***

-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -w #-}
module IdrisSetup (main) where

#if !defined(MIN_VERSION_Cabal)
# define MIN_VERSION_Cabal(x,y,z) 0
#endif

#if !defined(MIN_VERSION_base)
# define MIN_VERSION_base(x,y,z) 0
#endif

import Control.Monad
import Data.IORef
import Control.Exception (SomeException, catch)
import Data.String (fromString)

import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.InstallDirs as I
import Distribution.Simple.LocalBuildInfo as L
import qualified Distribution.Simple.Setup as S
import qualified Distribution.Simple.Program as P
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, notice, installOrdinaryFiles)
import Distribution.Simple.Utils (rewriteFileEx)
import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.Text
#if MIN_VERSION_Cabal(3,11,0)
import Distribution.Utils.Path
  (getSymbolicPath, makeSymbolicPath)
#endif

import System.Environment
import System.Exit
import System.FilePath ((</>), splitDirectories,isAbsolute)
import System.Directory
import qualified System.FilePath.Posix as Px
import System.Process

-- This is difference from vanilla idris-1.1.1
configConfigurationsFlags :: S.ConfigFlags -> [(FlagName, Bool)]
#if MIN_VERSION_Cabal(2,1,0)
configConfigurationsFlags = unFlagAssignment . S.configConfigurationsFlags
#else
configConfigurationsFlags = S.configConfigurationsFlags
#endif

#if !MIN_VERSION_base(4,6,0)
lookupEnv :: String -> IO (Maybe String)
lookupEnv v = lookup v `fmap` getEnvironment
#endif

-- After Idris is built, we need to check and install the prelude and other libs

-- -----------------------------------------------------------------------------
-- Idris Command Path

-- make on mingw32 expects unix style separators
#ifdef mingw32_HOST_OS
idrisCmd local = Px.joinPath $ splitDirectories $ ".." Px.</> ".." Px.</> bd Px.</> "idris" Px.</> "idris"
#else
idrisCmd local = ".." </> ".." </> bd </> "idris" </> "idris"
#endif
  where
    bd =
#if MIN_VERSION_Cabal(3,11,0)
        getSymbolicPath $
#endif
        buildDir local

-- -----------------------------------------------------------------------------
-- Make Commands

-- use GNU make on FreeBSD
#if defined(freebsd_HOST_OS) || defined(dragonfly_HOST_OS)\
    || defined(openbsd_HOST_OS) || defined(netbsd_HOST_OS)
mymake = "gmake"
#else
mymake = "make"
#endif

make verbosity dir args =
  P.runProgramInvocation verbosity $ P.simpleProgramInvocation mymake $
    [ "-C", dir ] ++ args

#ifdef mingw32_HOST_OS
windres verbosity =
  P.runProgramInvocation verbosity . P.simpleProgramInvocation "windres"
#endif
-- -----------------------------------------------------------------------------
-- Flags

usesGMP :: S.ConfigFlags -> Bool
usesGMP flags =
  case lookup (mkFlagName "gmp") (configConfigurationsFlags flags) of
    Just True -> True
    Just False -> False
    Nothing -> False

execOnly :: S.ConfigFlags -> Bool
execOnly flags =
  case lookup (mkFlagName "execonly") (configConfigurationsFlags flags) of
    Just True -> True
    Just False -> False
    Nothing -> False

isRelease :: S.ConfigFlags -> Bool
isRelease flags =
    case lookup (mkFlagName "release") (configConfigurationsFlags flags) of
      Just True -> True
      Just False -> False
      Nothing -> False

isFreestanding :: S.ConfigFlags -> Bool
isFreestanding flags =
  case lookup (mkFlagName "freestanding") (configConfigurationsFlags flags) of
    Just True -> True
    Just False -> False
    Nothing -> False

#if !(MIN_VERSION_Cabal(2,0,0))
mkFlagName :: String -> FlagName
mkFlagName = FlagName
#endif

-- -----------------------------------------------------------------------------
-- Clean

idrisClean _ flags _ _ = cleanStdLib
   where
      verbosity = S.fromFlag $ S.cleanVerbosity flags

      cleanStdLib = makeClean "libs"

      makeClean dir = make verbosity dir [ "clean", "IDRIS=idris" ]

-- -----------------------------------------------------------------------------
-- Configure

gitHash :: IO String
gitHash = do h <- Control.Exception.catch (readProcess "git" ["rev-parse", "--short", "HEAD"] "")
                  (\e -> let e' = (e :: SomeException) in return "PRE")
             return $ takeWhile (/= '\n') h

-- Put the Git hash into a module for use in the program
-- For release builds, just put the empty string in the module
generateVersionModule verbosity dir release = do
    hash <- gitHash
    let versionModulePath = dir </> "Version_idris" Px.<.> "hs"
    putStrLn $ "Generating " ++ versionModulePath ++
             if release then " for release" else " for prerelease " ++ hash
    createDirectoryIfMissingVerbose verbosity True dir
    rewriteFileEx verbosity versionModulePath (versionModuleContents hash)

  where versionModuleContents h = "module Version_idris where\n\n" ++
                                  "gitHash :: String\n" ++
                                  if release
                                    then "gitHash = \"\"\n"
                                    else "gitHash = \"git:" ++ h ++ "\"\n"

-- Generate a module that contains the lib path for a freestanding Idris
generateTargetModule verbosity dir targetDir = do
    let absPath = isAbsolute targetDir
    let targetModulePath = dir </> "Target_idris" Px.<.> "hs"
    putStrLn $ "Generating " ++ targetModulePath
    createDirectoryIfMissingVerbose verbosity True dir
    rewriteFileEx verbosity targetModulePath (versionModuleContents absPath targetDir)
            where versionModuleContents absolute td = "module Target_idris where\n\n" ++
                                    "import System.FilePath\n" ++
                                    "import System.Environment\n" ++
                                    "getDataDir :: IO String\n" ++
                                    if absolute
                                        then "getDataDir = return \"" ++ td ++ "\"\n"
                                        else "getDataDir = do \n" ++
                                             "   expath <- getExecutablePath\n" ++
                                             "   execDir <- return $ dropFileName expath\n" ++
                                             "   return $ execDir ++ \"" ++ td ++ "\"\n"
                                    ++ "getDataFileName :: FilePath -> IO FilePath\n"
                                    ++ "getDataFileName name = do\n"
                                    ++ "   dir <- getDataDir\n"
                                    ++ "   return (dir ++ \"/\" ++ name)"

-- a module that has info about existence and location of a bundled toolchain
generateToolchainModule verbosity srcDir toolDir = do
    let commonContent = "module Tools_idris where\n\n"
    let toolContent = case toolDir of
                        Just dir -> "hasBundledToolchain = True\n" ++
                                    "getToolchainDir = \"" ++ dir ++ "\"\n"
                        Nothing -> "hasBundledToolchain = False\n" ++
                                   "getToolchainDir = \"\""
    let toolPath = srcDir </> "Tools_idris" Px.<.> "hs"
    createDirectoryIfMissingVerbose verbosity True srcDir
    rewriteFileEx verbosity toolPath (commonContent ++ toolContent)

idrisConfigure _ flags pkgdesc local = do
    configureRTS
    withLibLBI pkgdesc local $ \_ libcfg -> do
      let libAutogenDir =
#if MIN_VERSION_Cabal(3,11,0)
            getSymbolicPath $
#endif
            autogenComponentModulesDir local libcfg
      generateVersionModule verbosity libAutogenDir (isRelease (configFlags local))
      if isFreestanding $ configFlags local
          then do
                  toolDir <- lookupEnv "IDRIS_TOOLCHAIN_DIR"
                  generateToolchainModule verbosity libAutogenDir toolDir
                  targetDir <- lookupEnv "IDRIS_LIB_DIR"
                  case targetDir of
                       Just d -> generateTargetModule verbosity libAutogenDir d
                       Nothing -> error $ "Trying to build freestanding without a target directory."
                                    ++ " Set it by defining IDRIS_LIB_DIR."
          else
                  generateToolchainModule verbosity libAutogenDir Nothing
    where
      verbosity = S.fromFlag $ S.configVerbosity flags
      version   = pkgVersion . package $ localPkgDescr local

      -- This is a hack. I don't know how to tell cabal that a data file needs
      -- installing but shouldn't be in the distribution. And it won't make the
      -- distribution if it's not there, so instead I just delete
      -- the file after configure.
      configureRTS = make verbosity "rts" ["clean"]

#if !(MIN_VERSION_Cabal(2,0,0))
      autogenComponentModulesDir lbi _ = autogenModulesDir lbi
#endif

#if !MIN_VERSION_Cabal(3,0,0)
idrisPreSDist args flags = do
  let dir = S.fromFlag (S.sDistDirectory flags)
  let verb = S.fromFlag (S.sDistVerbosity flags)
  generateVersionModule verb "src" True
  generateTargetModule verb "src" "./libs"
  generateToolchainModule verb "src" Nothing
  preSDist simpleUserHooks args flags

idrisSDist sdist pkgDesc bi hooks flags = do
  pkgDesc' <- addGitFiles pkgDesc
  sdist pkgDesc' bi hooks flags
    where
      addGitFiles :: PackageDescription -> IO PackageDescription
      addGitFiles pkgDesc = do
        files <- gitFiles
        return $ pkgDesc { extraSrcFiles = extraSrcFiles pkgDesc ++ files}
      gitFiles :: IO [FilePath]
      gitFiles = liftM lines (readProcess "git" ["ls-files"] "")

idrisPostSDist args flags desc lbi = do
  Control.Exception.catch (do let file = "src" </> "Version_idris" Px.<.> "hs"
                              let targetFile = "src" </> "Target_idris" Px.<.> "hs"
                              putStrLn $ "Removing generated modules:\n "
                                        ++ file ++ "\n" ++ targetFile
                              removeFile file
                              removeFile targetFile)
             (\e -> let e' = (e :: SomeException) in return ())
  postSDist simpleUserHooks args flags desc lbi
#endif

-- -----------------------------------------------------------------------------
-- Build

getVersion :: Args -> S.BuildFlags -> IO HookedBuildInfo
getVersion args flags = do
      hash <- gitHash
      let buildinfo = (emptyBuildInfo { cppOptions = ["-DVERSION="++hash] }) :: BuildInfo
      return (Just buildinfo, [])

idrisPreBuild args flags = do
#ifdef mingw32_HOST_OS
        createDirectoryIfMissingVerbose verbosity True dir
        windres verbosity ["icons/idris_icon.rc","-o", dir++"/idris_icon.o"]
        return (Nothing, [(fromString "idris", emptyBuildInfo { ldOptions = [dir ++ "/idris_icon.o"] })])
     where
        verbosity = S.fromFlag $ S.buildVerbosity flags

        dir =
#if MIN_VERSION_Cabal(3,11,0)
           getSymbolicPath $ S.fromFlagOrDefault (makeSymbolicPath "dist") $
#else
           S.fromFlagOrDefault "dist" $
#endif
           S.buildDistPref flags
#else
        return (Nothing, [])
#endif

idrisBuild _ flags _ local
   = if (execOnly (configFlags local)) then buildRTS
        else do buildStdLib
                buildRTS
   where
      verbosity = S.fromFlag $ S.buildVerbosity flags

      buildStdLib = do
            putStrLn "Building libraries..."
            makeBuild "libs"
         where
            makeBuild dir = make verbosity dir ["IDRIS=" ++ idrisCmd local]

      buildRTS = make verbosity "rts" $ gmpflag (usesGMP (configFlags local))

      gmpflag False = []
      gmpflag True = ["GMP=-DIDRIS_GMP"]

-- -----------------------------------------------------------------------------
-- Copy/Install

idrisInstall verbosity copy pkg local
   = if (execOnly (configFlags local)) then installRTS
        else do installStdLib
                installRTS
                installManPage
   where
      target = datadir $ L.absoluteInstallDirs pkg local copy

      installStdLib = do
        let target' = target -- </> "libs"
        putStrLn $ "Installing libraries in " ++ target'
        makeInstall "libs" target'

      installRTS = do
         let target' = target </> "rts"
         putStrLn $ "Installing run time system in " ++ target'
         makeInstall "rts" target'

      installManPage = do
         let mandest = mandir (L.absoluteInstallDirs pkg local copy) ++ "/man1"
         notice verbosity $ unwords ["Copying man page to", mandest]
         installOrdinaryFiles verbosity mandest [("man", "idris.1")]

      makeInstall src target =
         make verbosity src [ "install", "TARGET=" ++ target, "IDRIS=" ++ idrisCmd local]

-- -----------------------------------------------------------------------------
-- Test

-- There are two "dataDir" in cabal, and they don't relate to each other.
-- When fetching modules, idris uses the second path (in the pkg record),
-- which by default is the root folder of the project.
-- We want it to be the install directory where we put the idris libraries.
fixPkg pkg target = pkg { dataDir = target }

idrisTestHook args pkg local hooks flags = do
  let target =
#if MIN_VERSION_Cabal(3,11,0)
        makeSymbolicPath $
#endif
        datadir $ L.absoluteInstallDirs pkg local NoCopyDest
  testHook simpleUserHooks args (fixPkg pkg target) local hooks flags

-- -----------------------------------------------------------------------------
-- Main

-- Install libraries during both copy and install
-- See https://github.com/haskell/cabal/issues/709
main = defaultMainWithHooks $ simpleUserHooks
   { postClean = idrisClean
   , postConf  = idrisConfigure
   , preBuild = idrisPreBuild
   , postBuild = idrisBuild
   , postCopy = \_ flags pkg local ->
                  idrisInstall (S.fromFlag $ S.copyVerbosity flags)
                               (S.fromFlag $ S.copyDest flags) pkg local
   , postInst = \_ flags pkg local ->
                  idrisInstall (S.fromFlag $ S.installVerbosity flags)
                               NoCopyDest pkg local
#if !MIN_VERSION_Cabal(3,0,0)
   , preSDist = idrisPreSDist
   , sDistHook = idrisSDist (sDistHook simpleUserHooks)
   , postSDist = idrisPostSDist
#endif
   , testHook = idrisTestHook
   }
