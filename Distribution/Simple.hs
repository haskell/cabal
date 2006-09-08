{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple
-- Copyright   :  Isaac Jones 2003-2005
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Simple build system; basically the interface for
-- Distribution.Simple.\* modules.  When given the parsed command-line
-- args and package information, is able to perform basic commands
-- like configure, build, install, register, etc.
--
-- This module isn't called \"Simple\" because it's simple.  Far from
-- it.  It's called \"Simple\" because it does complicated things to
-- simple software.

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

module Distribution.Simple (
	module Distribution.Package,
	module Distribution.Version,
	module Distribution.License,
	module Distribution.Compiler,
	module Language.Haskell.Extension,
        -- * Simple interface
	defaultMain, defaultMainNoRead, defaultMainArgs,
        -- * Customization
        UserHooks(..), Args,
        defaultMainWithHooks, defaultUserHooks, emptyUserHooks,
        defaultHookedPackageDesc
#ifdef DEBUG        
        ,simpleHunitTests
#endif
  ) where

-- local
import Distribution.Compiler
import Distribution.Package --must not specify imports, since we're exporting moule.
import Distribution.PackageDescription
import Distribution.Program(lookupProgram, Program(..), ProgramConfiguration(..),
                            haddockProgram, rawSystemProgram, defaultProgramConfiguration,
                            pfesetupProgram, updateProgram,  rawSystemProgramConf)
import Distribution.PreProcess (knownSuffixHandlers, ppSuffixes, ppCpp',
                                ppUnlit, removePreprocessedPackage,
                                preprocessSources, PPSuffixHandler)
import Distribution.Setup

import Distribution.Simple.Build	( build )
import Distribution.Simple.SrcDist	( sdist )
import Distribution.Simple.Register	( register, unregister,
                                          writeInstalledConfig, installedPkgConfigFile,
                                          regScriptLocation, unregScriptLocation
                                        )

import Distribution.Simple.Configure(getPersistBuildConfig, maybeGetPersistBuildConfig,
                                     findProgram, configure, writePersistBuildConfig,
                                     localBuildInfoFile)

import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Install(install)
import Distribution.Simple.Utils (die, currentDir, rawSystemVerbose,
                                  defaultPackageDesc, defaultHookedPackageDesc,
                                  moduleToFilePath, findFile,
                                  distPref, srcPref, haddockPref)

#if mingw32_HOST_OS || mingw32_TARGET_OS
import Distribution.Simple.Utils (rawSystemPath)
#endif
import Language.Haskell.Extension
-- Base
import System.Environment(getArgs)
import System.Exit(ExitCode(..), exitWith)
import System.Directory(removeFile, doesFileExist, doesDirectoryExist)

import Distribution.License
import Control.Monad(when, unless)
import Data.List	( intersperse, unionBy )
import Data.Maybe       ( isJust, fromJust )
import System.IO.Error (try)
import Distribution.GetOpt

import Distribution.Compat.Directory(createDirectoryIfMissing,removeDirectoryRecursive, copyFile)
import Distribution.Compat.FilePath(joinFileName, joinPaths, joinFileExt,
                                    splitFileName, splitFileExt, changeFileExt)

#ifdef DEBUG
import HUnit (Test)
import Distribution.Version hiding (hunitTests)
#else
import Distribution.Version
#endif

type Args = [String]

-- | WARNING: The hooks interface is under rather constant flux as we
-- try to understand users needs.  Setup files that depend on this
-- interface may break in future releases.  Hooks allow authors to add
-- specific functionality before and after a command is run, and also
-- to specify additional preprocessors.
data UserHooks = UserHooks
    {
     runTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ExitCode, -- ^Used for @.\/setup test@
     readDesc :: IO (Maybe PackageDescription), -- ^Read the description file
     hookedPreProcessors :: [ PPSuffixHandler ],
        -- ^Custom preprocessors in addition to and overriding 'knownSuffixHandlers'.
     hookedPrograms :: [Program],
        -- ^These programs are detected at configure time.  Arguments for them are added to the configure command.

      -- |Hook to run before configure command
     preConf  :: Args -> ConfigFlags -> IO HookedBuildInfo,
     -- |Over-ride this hook to get different behavior during configure.
     confHook :: PackageDescription -> ConfigFlags -> IO LocalBuildInfo,
      -- |Hook to run after configure command
     postConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before build command.  Second arg indicates verbosity level.
     preBuild  :: Args -> BuildFlags -> IO HookedBuildInfo,

     -- |Over-ride this hook to get different behavior during build.
     buildHook :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> BuildFlags -> IO (),
      -- |Hook to run after build command.  Second arg indicates verbosity level.
     postBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before clean command.  Second arg indicates verbosity level.
     preClean  :: Args -> CleanFlags -> IO HookedBuildInfo,
     -- |Over-ride this hook to get different behavior during clean.
     cleanHook :: PackageDescription -> Maybe LocalBuildInfo -> Maybe UserHooks -> CleanFlags -> IO (),
      -- |Hook to run after clean command.  Second arg indicates verbosity level.
     postClean :: Args -> CleanFlags -> PackageDescription -> Maybe LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before copy command
     preCopy  :: Args -> CopyFlags -> IO HookedBuildInfo,
     -- |Over-ride this hook to get different behavior during copy.
     copyHook :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> CopyFlags -> IO (),
      -- |Hook to run after copy command
     postCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before install command
     preInst  :: Args -> InstallFlags -> IO HookedBuildInfo,

     -- |Over-ride this hook to get different behavior during install.
     instHook :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> InstallFlags -> IO (),
      -- |Hook to run after install command.  postInst should be run
      -- on the target, not on the build machine.
     postInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before sdist command.  Second arg indicates verbosity level.
     preSDist  :: Args -> SDistFlags -> IO HookedBuildInfo,
     -- |Over-ride this hook to get different behavior during sdist.
     sDistHook :: PackageDescription -> Maybe LocalBuildInfo -> Maybe UserHooks -> SDistFlags -> IO (),
      -- |Hook to run after sdist command.  Second arg indicates verbosity level.
     postSDist :: Args -> SDistFlags -> PackageDescription -> Maybe LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before register command
     preReg  :: Args -> RegisterFlags -> IO HookedBuildInfo,
     -- |Over-ride this hook to get different behavior during pfe.
     regHook :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> RegisterFlags -> IO (),
      -- |Hook to run after register command
     postReg :: Args -> RegisterFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before unregister command
     preUnreg  :: Args -> RegisterFlags -> IO HookedBuildInfo,
      -- |Over-ride this hook to get different behavior during pfe.
     unregHook :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> RegisterFlags -> IO (),
      -- |Hook to run after unregister command
     postUnreg :: Args -> RegisterFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before haddock command.  Second arg indicates verbosity level.
     preHaddock  :: Args -> HaddockFlags -> IO HookedBuildInfo,
      -- |Hook to run after haddock command.  Second arg indicates verbosity level.
     -- |Over-ride this hook to get different behavior during haddock.
     haddockHook :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> HaddockFlags -> IO (),
     postHaddock :: Args -> HaddockFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before pfe command.  Second arg indicates verbosity level.
     prePFE  :: Args -> PFEFlags -> IO HookedBuildInfo,
     -- |Over-ride this hook to get different behavior during pfe.
     pfeHook :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> PFEFlags -> IO (),
      -- |Hook to run after  pfe command.  Second arg indicates verbosity level.
     postPFE :: Args -> PFEFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode

    }

-- |A simple implementation of @main@ for a Cabal setup script.
-- It reads the package description file using IO, and performs the
-- action specified on the command line.
defaultMain :: IO ()
defaultMain = getArgs >>=defaultMainArgs

defaultMainArgs :: [String] -> IO ()
defaultMainArgs args = do
                 (action, args) <- parseGlobalArgs (allPrograms Nothing) args
                 pkg_descr_file <- defaultPackageDesc
                 pkg_descr <- readPackageDescription pkg_descr_file
                 defaultMainWorker pkg_descr action args Nothing
                 return ()

-- | A customizable version of 'defaultMain'.
defaultMainWithHooks :: UserHooks -> IO ()
defaultMainWithHooks hooks
    = do args <- getArgs
         (action, args) <- parseGlobalArgs (allPrograms (Just hooks)) args
         maybeDesc <- readDesc hooks
         pkg_descr <- maybe (defaultPackageDesc >>= readPackageDescription)
                            return maybeDesc
         defaultMainWorker pkg_descr action args (Just hooks)
         return ()

-- |Like 'defaultMain', but accepts the package description as input
-- rather than using IO to read it.
defaultMainNoRead :: PackageDescription -> IO ()
defaultMainNoRead pkg_descr
    = do args <- getArgs
         (action, args) <- parseGlobalArgs (allPrograms Nothing) args
         defaultMainWorker pkg_descr action args Nothing
         return ()

-- |Combine the programs in the given hooks with the programs built
-- into cabal.
allPrograms :: Maybe UserHooks
            -> ProgramConfiguration -- combine defaults w/ user programs
allPrograms Nothing = defaultProgramConfiguration
allPrograms (Just h) = foldl (\pConf p -> updateProgram (Just p) pConf)
                        defaultProgramConfiguration
                        (hookedPrograms h)

-- |Combine the preprocessors in the given hooks with the
-- preprocessors built into cabal.
allSuffixHandlers :: Maybe UserHooks
                  -> [PPSuffixHandler]
allSuffixHandlers hooks
    = maybe knownSuffixHandlers
      (\h -> overridesPP (hookedPreProcessors h) knownSuffixHandlers)
      hooks
    where
      overridesPP :: [PPSuffixHandler] -> [PPSuffixHandler] -> [PPSuffixHandler]
      overridesPP = unionBy (\x y -> fst x == fst y)

-- |Helper function for /defaultMain/ and /defaultMainNoRead/
defaultMainWorker :: PackageDescription
                  -> Action
                  -> [String] -- ^args1
                  -> Maybe UserHooks
                  -> IO ExitCode
defaultMainWorker pkg_descr_in action args hooks
    = do case action of
            ConfigCmd flags -> do
                (flags, optFns, args) <-
			parseConfigureArgs (allPrograms hooks) flags args [buildDirOpt]
                pkg_descr <- hookOrInArgs preConf args flags
                (warns, ers) <- sanityCheckPackage pkg_descr
                errorOut warns ers

                let c = maybe (confHook defaultUserHooks) confHook hooks
		localbuildinfo <- c pkg_descr flags
		writePersistBuildConfig (foldr id localbuildinfo optFns)
                postHook postConf args flags pkg_descr localbuildinfo

            BuildCmd -> do
                (flags, _, args) <- parseBuildArgs args []
                pkg_descr <- hookOrInArgs preBuild args flags
		localbuildinfo <- getPersistBuildConfig

                cmdHook buildHook pkg_descr localbuildinfo flags
                postHook postBuild args flags pkg_descr localbuildinfo

            HaddockCmd -> do
                (verbose, _, args) <- parseHaddockArgs emptyHaddockFlags args []
                pkg_descr <- hookOrInArgs preHaddock args verbose
		localbuildinfo <- getPersistBuildConfig

                cmdHook haddockHook pkg_descr localbuildinfo verbose
                postHook postHaddock args verbose pkg_descr localbuildinfo

            ProgramaticaCmd -> do
                (verbose, _, args) <- parseProgramaticaArgs args []
                pkg_descr <- hookOrInArgs prePFE args verbose
                localbuildinfo <- getPersistBuildConfig

                cmdHook pfeHook pkg_descr localbuildinfo verbose
                postHook postPFE args verbose pkg_descr localbuildinfo

            CleanCmd -> do
                (verbose,_, args) <- parseCleanArgs args []
                pkg_descr <- hookOrInArgs preClean args verbose
		maybeLocalbuildinfo <- maybeGetPersistBuildConfig

                cmdHook cleanHook pkg_descr maybeLocalbuildinfo verbose
                postHook postClean args verbose pkg_descr maybeLocalbuildinfo

            CopyCmd mprefix -> do
                (flags, _, args) <- parseCopyArgs (CopyFlags mprefix 0) args []
                pkg_descr <- hookOrInArgs preCopy args flags
		localbuildinfo <- getPersistBuildConfig

                cmdHook copyHook pkg_descr localbuildinfo flags
                postHook postCopy args flags pkg_descr localbuildinfo

            InstallCmd -> do
                (flags, _, args) <- parseInstallArgs emptyInstallFlags args []
                pkg_descr <- hookOrInArgs preInst args flags
		localbuildinfo <- getPersistBuildConfig

                cmdHook instHook pkg_descr localbuildinfo flags
                postHook postInst args flags pkg_descr localbuildinfo

            SDistCmd -> do
                (flags,_, args) <- parseSDistArgs args []
                pkg_descr <- hookOrInArgs preSDist args flags
                maybeLocalbuildinfo <- maybeGetPersistBuildConfig

                cmdHook sDistHook pkg_descr maybeLocalbuildinfo flags
                postHook postSDist args flags pkg_descr maybeLocalbuildinfo

            TestCmd -> do
                (verbose,_, args) <- parseTestArgs args []
                case hooks of
                 Nothing -> return ExitSuccess
                 Just h  -> do localbuildinfo <- getPersistBuildConfig
                               out <- (runTests h) args False pkg_descr_in localbuildinfo
                               when (isFailure out) (exitWith out)
                               return out

            RegisterCmd  -> do
                (flags, _, args) <- parseRegisterArgs emptyRegisterFlags args []
                pkg_descr <- hookOrInArgs preReg args flags
		localbuildinfo <- getPersistBuildConfig

                cmdHook regHook pkg_descr localbuildinfo flags 
                postHook postReg args flags pkg_descr localbuildinfo

            UnregisterCmd -> do
                (flags,_, args) <- parseUnregisterArgs emptyRegisterFlags args []
                pkg_descr <- hookOrInArgs preUnreg args flags
		localbuildinfo <- getPersistBuildConfig

                cmdHook unregHook pkg_descr localbuildinfo flags
                postHook postUnreg args flags pkg_descr localbuildinfo

            HelpCmd -> return ExitSuccess -- this is handled elsewhere
        where
        hookOrInArgs :: (UserHooks -> ([String] -> b -> IO HookedBuildInfo))
                     -> [String]
                     -> b
                     -> IO PackageDescription
        hookOrInArgs f a i
                 = case hooks of
                    Nothing -> no_extra_flags a >> return pkg_descr_in
                    Just h -> do pbi <- f h a i
                                 return (updatePackageDescription pbi pkg_descr_in)
        cmdHook f desc lbi = (maybe (f defaultUserHooks) f hooks) desc lbi hooks
        postHook f args flags pkg_descr localbuildinfo
                 = case hooks of
                    Nothing -> return ExitSuccess
                    Just h  -> f h args flags pkg_descr localbuildinfo

        isFailure :: ExitCode -> Bool
        isFailure (ExitFailure _) = True
        isFailure _               = False

-- (filter (\x -> notElem x overriders) overridden) ++ overriders


getModulePaths :: BuildInfo -> [String] -> IO [FilePath]
getModulePaths bi =
   fmap concat .
      mapM (flip (moduleToFilePath (hsSourceDirs bi)) ["hs", "lhs"])

haddock :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> HaddockFlags -> IO ()
haddock pkg_descr lbi hooks (HaddockFlags hoogle verbose) = do
    let pps = allSuffixHandlers hooks
    confHaddock <- do let programConf = withPrograms lbi
                      let haddockName = programName haddockProgram
                      mHaddock <- lookupProgram haddockName programConf
                      maybe (die "haddock command not found") return mHaddock

    let tmpDir = joinPaths (buildDir lbi) "tmp"
    createDirectoryIfMissing True tmpDir
    createDirectoryIfMissing True haddockPref
    preprocessSources pkg_descr lbi verbose pps

    setupMessage "Running Haddock for" pkg_descr

    let replaceLitExts = map (joinFileName tmpDir . flip changeFileExt "hs")
    let mockAll bi = mapM_ (mockPP ["-D__HADDOCK__"] pkg_descr bi lbi tmpDir verbose)
    let showPkg     = showPackageId (package pkg_descr)
    let showDepPkgs = map showPackageId (packageDeps lbi)
    let outputFlag  = if hoogle then "--hoogle" else "--html"

    withLib pkg_descr () $ \lib -> do
        let bi = libBuildInfo lib
        inFiles <- getModulePaths bi (exposedModules lib ++ otherModules bi)
        mockAll bi inFiles
        let prologName = showPkg ++ "-haddock-prolog.txt"
        writeFile prologName (description pkg_descr ++ "\n")
        let outFiles = replaceLitExts inFiles
        let haddockFile = joinFileName haddockPref (haddockName pkg_descr)
        -- FIX: replace w/ rawSystemProgramConf?
        rawSystemProgram verbose confHaddock
                ([outputFlag,
                  "--odir=" ++ haddockPref,
                  "--title=" ++ showPkg ++ ": " ++ synopsis pkg_descr,
                  "--package=" ++ showPkg,
                  "--dump-interface=" ++ haddockFile,
                  "--prologue=" ++ prologName]
                 ++ map ("--use-package=" ++) showDepPkgs
                 ++ programArgs confHaddock
                 ++ (if verbose > 4 then ["--verbose"] else [])
                 ++ outFiles
                 ++ map ("--hide=" ++) (otherModules bi)
                )
        removeFile prologName
    withExe pkg_descr $ \exe -> do
        let bi = buildInfo exe
            exeTargetDir = haddockPref `joinFileName` exeName exe
        createDirectoryIfMissing True exeTargetDir
        inFiles' <- getModulePaths bi (otherModules bi)
        srcMainPath <- findFile (hsSourceDirs bi) (modulePath exe)
        let inFiles = srcMainPath : inFiles'
        mockAll bi inFiles
        let outFiles = replaceLitExts inFiles
        rawSystemProgram verbose confHaddock
                ([outputFlag,
                  "--odir=" ++ exeTargetDir,
                  "--title=" ++ exeName exe]
                 ++ map ("--use-package=" ++) (showPkg:showDepPkgs)
                 ++ programArgs confHaddock
                 ++ (if verbose > 4 then ["--verbose"] else [])
                 ++ outFiles
                )

    removeDirectoryRecursive tmpDir
  where
        mockPP inputArgs pkg_descr bi lbi pref verbose file
            = do let (filePref, fileName) = splitFileName file
                 let targetDir = joinPaths pref filePref
                 let targetFile = joinFileName targetDir fileName
                 let (targetFileNoext, targetFileExt) = splitFileExt targetFile
                 createDirectoryIfMissing True targetDir
                 if (needsCpp pkg_descr)
                    then ppCpp' inputArgs bi lbi file targetFile verbose
                    else copyFile file targetFile >> return ExitSuccess
                 when (targetFileExt == "lhs") $ do
                       ppUnlit targetFile (joinFileExt targetFileNoext "hs") verbose
                       return ()
        needsCpp :: PackageDescription -> Bool
        needsCpp p =
           hasLibs p &&
           any (== CPP) (extensions $ libBuildInfo $ fromJust $ library p)

pfe :: PackageDescription -> LocalBuildInfo -> Maybe UserHooks -> PFEFlags -> IO ()
pfe pkg_descr _lbi hooks (PFEFlags verbose) = do
    let pps = allSuffixHandlers hooks
    unless (hasLibs pkg_descr) $
        die "no libraries found in this project"
    withLib pkg_descr () $ \lib -> do
        lbi <- getPersistBuildConfig
        let bi = libBuildInfo lib
        let mods = exposedModules lib ++ otherModules (libBuildInfo lib)
        preprocessSources pkg_descr lbi verbose pps
        inFiles <- getModulePaths bi mods
        rawSystemProgramConf verbose (programName pfesetupProgram) (withPrograms lbi)
                ("noplogic":"cpp": (if verbose > 4 then ["-v"] else [])
                ++ inFiles)
        return ()

clean :: PackageDescription -> Maybe LocalBuildInfo -> Maybe UserHooks -> CleanFlags -> IO ()
clean pkg_descr maybeLbi hooks (CleanFlags verbose) = do
    let pps = allSuffixHandlers hooks
    putStrLn "cleaning..."
    try $ removeDirectoryRecursive (joinPaths distPref "doc")
    try $ removeFile installedPkgConfigFile
    try $ removeFile localBuildInfoFile
    try $ removeFile regScriptLocation
    try $ removeFile unregScriptLocation
    removePreprocessedPackage pkg_descr currentDir (ppSuffixes pps)
    mapM_ removeFileOrDirectory (extraTmpFiles pkg_descr)

    when (isJust maybeLbi) $ do
        let lbi = fromJust maybeLbi
        try $ removeDirectoryRecursive (buildDir lbi)
        case compilerFlavor (compiler lbi) of
          GHC -> cleanGHCExtras lbi
          JHC -> cleanJHCExtras lbi
          _   -> return ()
  where
        cleanGHCExtras lbi = do
            -- remove source stubs for library
            withLib pkg_descr () $ \ Library{libBuildInfo=bi} ->
                removeGHCModuleStubs bi (libModules pkg_descr)
            -- remove source stubs for executables
            withExe pkg_descr $ \ Executable{modulePath=exeSrcName
                                            ,buildInfo=bi} -> do
                removeGHCModuleStubs bi (exeModules pkg_descr)
                let (startN, _) = splitFileExt exeSrcName
                try $ removeFile (startN ++ "_stub.h")
                try $ removeFile (startN ++ "_stub.c")
        removeGHCModuleStubs :: BuildInfo -> [String] -> IO ()
        removeGHCModuleStubs (BuildInfo{hsSourceDirs=dirs}) mods = do
            s <- mapM (\x -> moduleToFilePath dirs (x ++"_stub") ["h", "c"]) mods
            mapM_ removeFile (concat s)
        -- JHC FIXME remove exe-sources
        cleanJHCExtras lbi = do
            try $ removeFile (buildDir lbi `joinFileName` "jhc-pkg.conf")
            removePreprocessedPackage pkg_descr currentDir ["ho"]
        removeFileOrDirectory :: FilePath -> IO ()
        removeFileOrDirectory fname = do
            isDir <- doesDirectoryExist fname
            isFile <- doesFileExist fname
            if isDir then removeDirectoryRecursive fname
              else if isFile then removeFile fname
              else return ()

no_extra_flags :: [String] -> IO ()
no_extra_flags [] = return ()
no_extra_flags extra_flags  = 
  die ("Unrecognised flags: " ++ concat (intersperse "," extra_flags))

buildDirOpt :: OptDescr (LocalBuildInfo -> LocalBuildInfo)
buildDirOpt = Option "b" ["scratchdir"] (reqDirArg setBuildDir)
		"directory to receive the built package [dist/build]"
  where setBuildDir dir lbi = lbi { buildDir = dir }

-- |Empty 'UserHooks' which do nothing.
emptyUserHooks :: UserHooks
emptyUserHooks
    = UserHooks
      {
       runTests  = res,
       readDesc  = return Nothing,
       hookedPreProcessors = [],
       hookedPrograms      = [],
       preConf   = rn,
       confHook  = (\_ _ -> return (error "No local build info generated during configure. Over-ride empty configure hook.")),
       postConf  = res,
       preBuild  = rn,
       buildHook = ru,
       postBuild = res,
       preClean  = rn,
       cleanHook = ru,
       postClean = res,
       preCopy   = rn,
       copyHook  = ru,
       postCopy  = res,
       preInst   = rn,
       instHook  = ru,
       postInst  = res,
       preSDist  = rn,
       sDistHook = ru,
       postSDist = res,
       preReg    = rn,
       regHook   = ru,
       postReg   = res,
       preUnreg  = rn,
       unregHook = ru,
       postUnreg = res,
       prePFE    = rn,
       pfeHook   = ru,
       postPFE   = res,
       preHaddock  = rn,
       haddockHook = ru,
       postHaddock = res
      }
    where rn  _ _    = return emptyHookedBuildInfo
          res _ _ _ _  = return ExitSuccess
          ru _ _ _ _ = return ()

-- |Basic default 'UserHooks':
--
-- * on non-Windows systems, 'postConf' runs @.\/configure@, if present.
--
-- * the pre-hooks 'preBuild', 'preClean', 'preCopy', 'preInst',
--   'preReg' and 'preUnreg' read additional build information from
--   /package/@.buildinfo@, if present.
--
-- Thus @configure@ can use local system information to generate
-- /package/@.buildinfo@ and possibly other files.

-- FIXME: do something sensible for windows, or do nothing in postConf.

defaultUserHooks :: UserHooks
defaultUserHooks
    = emptyUserHooks
      {
       postConf  = defaultPostConf,
       confHook  = configure,
       preBuild  = readHook buildVerbose,
       buildHook = defaultBuildHook,
       preClean  = readHook cleanVerbose,
       preCopy   = readHook copyVerbose,
       copyHook  = \desc lbi _ f -> install desc lbi f, -- has correct 'copy' behavior with params
       preInst   = readHook installVerbose,
       instHook  = defaultInstallHook,
       sDistHook = \p _ h f -> sdist p f srcPref distPref (allSuffixHandlers h),
       pfeHook   = pfe,
       cleanHook = clean,
       haddockHook = haddock,
       preReg    = readHook regVerbose,
       regHook   = defaultRegHook,
       unregHook = \p l _ f -> unregister p l f,
       preUnreg  = readHook regVerbose
      }
    where defaultPostConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ExitCode
          defaultPostConf args flags pkg_descr lbi
              = do let verbose = configVerbose flags
                       args' = configureArgs flags ++ args
                   confExists <- doesFileExist "configure"
                   if confExists then
#if mingw32_HOST_OS || mingw32_TARGET_OS
                       -- FIXME: hack for script files under MinGW
                       -- This assumes sh (check for #! line?)
                       rawSystemPath verbose "sh" ("configure" : args')
#else
                       -- FIXME: should we really be discarding the exit code?
                       rawSystemVerbose verbose "./configure" args'
#endif
                     else do
                       no_extra_flags args
                       return ExitSuccess

          readHook :: (a -> Int) -> Args -> a -> IO HookedBuildInfo
          readHook verbose a flags = do
              no_extra_flags a
              maybe_infoFile <- defaultHookedPackageDesc
              case maybe_infoFile of
                  Nothing       -> return emptyHookedBuildInfo
                  Just infoFile -> do
                      when (verbose flags > 0) $
                          putStrLn $ "Reading parameters from " ++ infoFile
                      readHookedBuildInfo infoFile

defaultInstallHook :: PackageDescription -> LocalBuildInfo
	-> Maybe UserHooks ->InstallFlags -> IO ()
defaultInstallHook pkg_descr localbuildinfo _ (InstallFlags uInstFlag verbose) = do
  install pkg_descr localbuildinfo (CopyFlags NoCopyDest verbose)
  when (hasLibs pkg_descr) $
      register pkg_descr localbuildinfo 
           emptyRegisterFlags{ regUser=uInstFlag, regVerbose=verbose }

defaultBuildHook :: PackageDescription -> LocalBuildInfo
	-> Maybe UserHooks -> BuildFlags -> IO ()
defaultBuildHook pkg_descr localbuildinfo hooks flags = do
  build pkg_descr localbuildinfo flags (allSuffixHandlers hooks)
  when (hasLibs pkg_descr) $
      writeInstalledConfig pkg_descr localbuildinfo False

defaultRegHook :: PackageDescription -> LocalBuildInfo
	-> Maybe UserHooks -> RegisterFlags -> IO ()
defaultRegHook pkg_descr localbuildinfo _ flags =
    if hasLibs pkg_descr
    then register pkg_descr localbuildinfo flags
    else die "Package contains no library to register"

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
simpleHunitTests :: [Test]
simpleHunitTests = []
#endif
