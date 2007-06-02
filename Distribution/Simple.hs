{-# OPTIONS -cpp #-}
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
        defaultMainWithHooks, defaultMainWithHooksArgs,
        simpleUserHooks, defaultUserHooks, emptyUserHooks,
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
                                preprocessSources, PPSuffixHandler,
                                runSimplePreProcessor)
import Distribution.Setup

import Distribution.Simple.Build	( build, makefile )
import Distribution.Simple.SrcDist	( sdist )
import Distribution.Simple.Register	( register, unregister,
                                          writeInstalledConfig, installedPkgConfigFile,
                                          regScriptLocation, unregScriptLocation
                                        )

import Distribution.Simple.Configure(getPersistBuildConfig, maybeGetPersistBuildConfig,
                                     configure, writePersistBuildConfig, localBuildInfoFile,
                                     haddockVersion)

import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..), distPref, 
                                            srcPref, haddockPref, substDir )
import Distribution.Simple.Install(install)
import Distribution.Simple.Utils (die, currentDir,
                                  createDirectoryIfMissingVerbose,
                                  defaultPackageDesc, defaultHookedPackageDesc,
                                  moduleToFilePath, findFile, warn)

import Distribution.Simple.Utils (rawSystemPathExit, rawSystemStdout)
import Distribution.Verbosity
import Language.Haskell.Extension
-- Base
import System.Environment(getArgs)
import System.Exit(ExitCode(..), exitWith)
import System.Directory(removeFile, doesFileExist, doesDirectoryExist)

import Distribution.License
import Control.Monad(liftM, when, unless)
import Data.Char	( isSpace )
import Data.List	( intersperse, unionBy )
import Data.Maybe	( catMaybes )
import System.IO.Error (try)
import System.IO        ( hPutStrLn, stderr )
import System.Environment ( getProgName )
import Distribution.GetOpt

import Distribution.Compat.Directory(removeDirectoryRecursive, copyFile)
import System.FilePath((</>), (<.>), splitFileName, splitExtension , replaceExtension)

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
     runTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO (), -- ^Used for @.\/setup test@
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
     postConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO (),

      -- |Hook to run before build command.  Second arg indicates verbosity level.
     preBuild  :: Args -> BuildFlags -> IO HookedBuildInfo,

     -- |Over-ride this hook to gbet different behavior during build.
     buildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO (),
      -- |Hook to run after build command.  Second arg indicates verbosity level.
     postBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO (),

      -- |Hook to run before makefile command.  Second arg indicates verbosity level.
     preMakefile  :: Args -> MakefileFlags -> IO HookedBuildInfo,

     -- |Over-ride this hook to gbet different behavior during makefile.
     makefileHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> MakefileFlags -> IO (),
      -- |Hook to run after makefile command.  Second arg indicates verbosity level.
     postMakefile :: Args -> MakefileFlags -> PackageDescription -> LocalBuildInfo -> IO (),

      -- |Hook to run before clean command.  Second arg indicates verbosity level.
     preClean  :: Args -> CleanFlags -> IO HookedBuildInfo,
     -- |Over-ride this hook to get different behavior during clean.
     cleanHook :: PackageDescription -> Maybe LocalBuildInfo -> UserHooks -> CleanFlags -> IO (),
      -- |Hook to run after clean command.  Second arg indicates verbosity level.
     postClean :: Args -> CleanFlags -> PackageDescription -> Maybe LocalBuildInfo -> IO (),

      -- |Hook to run before copy command
     preCopy  :: Args -> CopyFlags -> IO HookedBuildInfo,
     -- |Over-ride this hook to get different behavior during copy.
     copyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO (),
      -- |Hook to run after copy command
     postCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO (),

      -- |Hook to run before install command
     preInst  :: Args -> InstallFlags -> IO HookedBuildInfo,

     -- |Over-ride this hook to get different behavior during install.
     instHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO (),
      -- |Hook to run after install command.  postInst should be run
      -- on the target, not on the build machine.
     postInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO (),

      -- |Hook to run before sdist command.  Second arg indicates verbosity level.
     preSDist  :: Args -> SDistFlags -> IO HookedBuildInfo,
     -- |Over-ride this hook to get different behavior during sdist.
     sDistHook :: PackageDescription -> Maybe LocalBuildInfo -> UserHooks -> SDistFlags -> IO (),
      -- |Hook to run after sdist command.  Second arg indicates verbosity level.
     postSDist :: Args -> SDistFlags -> PackageDescription -> Maybe LocalBuildInfo -> IO (),

      -- |Hook to run before register command
     preReg  :: Args -> RegisterFlags -> IO HookedBuildInfo,
     -- |Over-ride this hook to get different behavior during pfe.
     regHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO (),
      -- |Hook to run after register command
     postReg :: Args -> RegisterFlags -> PackageDescription -> LocalBuildInfo -> IO (),

      -- |Hook to run before unregister command
     preUnreg  :: Args -> RegisterFlags -> IO HookedBuildInfo,
      -- |Over-ride this hook to get different behavior during pfe.
     unregHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO (),
      -- |Hook to run after unregister command
     postUnreg :: Args -> RegisterFlags -> PackageDescription -> LocalBuildInfo -> IO (),

      -- |Hook to run before haddock command.  Second arg indicates verbosity level.
     preHaddock  :: Args -> HaddockFlags -> IO HookedBuildInfo,
      -- |Hook to run after haddock command.  Second arg indicates verbosity level.
     -- |Over-ride this hook to get different behavior during haddock.
     haddockHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> HaddockFlags -> IO (),
     postHaddock :: Args -> HaddockFlags -> PackageDescription -> LocalBuildInfo -> IO (),

      -- |Hook to run before pfe command.  Second arg indicates verbosity level.
     prePFE  :: Args -> PFEFlags -> IO HookedBuildInfo,
     -- |Over-ride this hook to get different behavior during pfe.
     pfeHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> PFEFlags -> IO (),
      -- |Hook to run after  pfe command.  Second arg indicates verbosity level.
     postPFE :: Args -> PFEFlags -> PackageDescription -> LocalBuildInfo -> IO ()

    }

-- | A simple implementation of @main@ for a Cabal setup script.
-- It reads the package description file using IO, and performs the
-- action specified on the command line.
defaultMain :: IO ()
defaultMain = defaultMain__ Nothing Nothing Nothing

-- | A version of 'defaultMain' that is passed the command line
-- arguments, rather than getting them from the environment.
defaultMainArgs :: [String] -> IO ()
defaultMainArgs args = defaultMain__ (Just args) Nothing Nothing

-- | A customizable version of 'defaultMain'.
defaultMainWithHooks :: UserHooks -> IO ()
defaultMainWithHooks hooks = defaultMain__ Nothing (Just hooks) Nothing

-- | A customizable version of 'defaultMain' that also takes the command
-- line arguments.
defaultMainWithHooksArgs :: UserHooks -> [String] -> IO ()
defaultMainWithHooksArgs hooks args
   = defaultMain__ (Just args) (Just hooks) Nothing

-- | Like 'defaultMain', but accepts the package description as input
-- rather than using IO to read it.
defaultMainNoRead :: PackageDescription -> IO ()
defaultMainNoRead pkg_descr = defaultMain__ Nothing Nothing (Just pkg_descr)

defaultMain__    :: Maybe [String]
	         -> Maybe UserHooks
		 -> Maybe PackageDescription
		 -> IO ()
defaultMain__ margs mhooks mdescr = do
   args <- maybe getArgs return margs
   let hooks = maybe simpleUserHooks id mhooks
   let prog_conf = allPrograms hooks
   (action, args') <- parseGlobalArgs prog_conf args
   let get_pkg_descr verbosity = 
	 case mdescr of
		Just pkg_descr -> return pkg_descr
		Nothing -> do
			    maybeDesc <- readDesc hooks
			    case maybeDesc of
				Nothing -> defaultPkgDescr
				Just p  -> return p
        where
	  defaultPkgDescr = do
		 pkg_descr_file <- defaultPackageDesc verbosity
         	 readPackageDescription verbosity pkg_descr_file
   defaultMainWorker get_pkg_descr action args' hooks prog_conf

-- | Combine the programs in the given hooks with the programs built
-- into cabal.
allPrograms :: UserHooks
            -> ProgramConfiguration -- combine defaults w/ user programs
allPrograms h = foldl (flip updateProgram) 
                      defaultProgramConfiguration
                      (hookedPrograms h)

-- | Combine the preprocessors in the given hooks with the
-- preprocessors built into cabal.
allSuffixHandlers :: UserHooks
                  -> [PPSuffixHandler]
allSuffixHandlers hooks
    = overridesPP (hookedPreProcessors hooks) knownSuffixHandlers
    where
      overridesPP :: [PPSuffixHandler] -> [PPSuffixHandler] -> [PPSuffixHandler]
      overridesPP = unionBy (\x y -> fst x == fst y)

-- | Helper function for /defaultMain/
defaultMainWorker :: (Verbosity -> IO PackageDescription)
                  -> Action
                  -> [String]
                  -> UserHooks
                  -> ProgramConfiguration
                  -> IO ()
defaultMainWorker get_pkg_descr action all_args hooks prog_conf
    = do case action of
            ConfigCmd flags -> do
                (flags', optFns, args) <-
			parseConfigureArgs prog_conf flags all_args [scratchDirOpt]
                pbi <- preConf hooks args flags'
                pkg_descr0 <- get_pkg_descr (configVerbose flags')
                let pkg_descr = updatePackageDescription pbi pkg_descr0
                (warns, ers) <- sanityCheckPackage pkg_descr
                errorOut (configVerbose flags') warns ers
		localbuildinfo <- confHook hooks pkg_descr flags'
		writePersistBuildConfig (foldr id localbuildinfo optFns)
                postConf hooks args flags' pkg_descr localbuildinfo

            BuildCmd -> 
                command (parseBuildArgs emptyBuildFlags) buildVerbose
                        preBuild buildHook postBuild
                        getPersistBuildConfig
        
            MakefileCmd ->
                command (parseMakefileArgs emptyMakefileFlags) makefileVerbose
                        preMakefile makefileHook postMakefile
                        getPersistBuildConfig
        
            HaddockCmd -> 
                command (parseHaddockArgs emptyHaddockFlags) haddockVerbose
                        preHaddock haddockHook postHaddock
                        getPersistBuildConfig

            ProgramaticaCmd -> do
                command parseProgramaticaArgs pfeVerbose
                        prePFE pfeHook postPFE
                        getPersistBuildConfig

            CleanCmd -> do
                command (parseCleanArgs emptyCleanFlags) cleanVerbose
                        preClean cleanHook postClean
                        maybeGetPersistBuildConfig

            CopyCmd mprefix -> do
                command (parseCopyArgs (emptyCopyFlags mprefix)) copyVerbose
                        preCopy copyHook postCopy
                        getPersistBuildConfig

            InstallCmd -> do
                command (parseInstallArgs emptyInstallFlags) installVerbose
                        preInst instHook postInst
                        getPersistBuildConfig

            SDistCmd -> do
                command parseSDistArgs sDistVerbose
                        preSDist sDistHook postSDist
                        maybeGetPersistBuildConfig

            TestCmd -> do
                (verbosity,_, args) <- parseTestArgs all_args []
                localbuildinfo <- getPersistBuildConfig
                pkg_descr <- get_pkg_descr verbosity
                runTests hooks args False pkg_descr localbuildinfo

            RegisterCmd  -> do
                command (parseRegisterArgs emptyRegisterFlags) regVerbose
                        preReg regHook postReg
                        getPersistBuildConfig

            UnregisterCmd -> do
                command (parseUnregisterArgs emptyRegisterFlags) regVerbose
                        preUnreg unregHook postUnreg
                        getPersistBuildConfig

            HelpCmd -> return () -- this is handled elsewhere
        where
        command parse_args get_verbosity
                pre_hook cmd_hook post_hook
                get_build_config = do
           (flags, _, args) <- parse_args all_args []
           pbi <- pre_hook hooks args flags
           pkg_descr0 <- get_pkg_descr (get_verbosity flags)
           let pkg_descr = updatePackageDescription pbi pkg_descr0
           localbuildinfo <- get_build_config
           cmd_hook hooks pkg_descr localbuildinfo hooks flags
           post_hook hooks args flags pkg_descr localbuildinfo


getModulePaths :: LocalBuildInfo -> BuildInfo -> [String] -> IO [FilePath]
getModulePaths lbi bi =
   fmap concat .
      mapM (flip (moduleToFilePath (buildDir lbi : hsSourceDirs bi)) ["hs", "lhs"])

-- --------------------------------------------------------------------------
-- Haddock support

haddock :: PackageDescription -> LocalBuildInfo -> UserHooks -> HaddockFlags -> IO ()
haddock pkg_descr lbi hooks (HaddockFlags hoogle html_loc verbosity) = do
    let pps = allSuffixHandlers hooks
    confHaddock <- do let programConf = withPrograms lbi
                      let haddockPath = programName haddockProgram
                      mHaddock <- lookupProgram haddockPath programConf
                      maybe (die "haddock command not found") return mHaddock

    let tmpDir = buildDir lbi </> "tmp"
    createDirectoryIfMissingVerbose verbosity True tmpDir
    createDirectoryIfMissingVerbose verbosity True $ haddockPref pkg_descr
    preprocessSources pkg_descr lbi verbosity pps

    setupMessage verbosity "Running Haddock for" pkg_descr

    let replaceLitExts = map ( (tmpDir </>) . (`replaceExtension` "hs") )
    let mockAll bi = mapM_ (mockPP ["-D__HADDOCK__"] bi tmpDir)
    let showPkg     = showPackageId (package pkg_descr)
    let outputFlag  = if hoogle then "--hoogle" else "--html"
    have_new_flags <- fmap (> Version [0,8] []) (haddockVersion verbosity lbi)
    let ghcpkgFlags = if have_new_flags
                      then ["--ghc-pkg=" ++ compilerPkgTool (compiler lbi)]
                      else []
    let allowMissingHtmlFlags = if have_new_flags
                                then ["--allow-missing-html"]
                                else []

    let pkgTool = compilerPkgTool (compiler lbi)
    let trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    let getField pkgId f = do
            let name = showPackageId pkgId
            s <- rawSystemStdout verbosity pkgTool ["field", name, f]
            return $ trim $ dropWhile (not . isSpace) $ head $ lines s
    let makeReadInterface pkgId = do
            interface <- getField pkgId "haddock-interfaces"
            html <- case html_loc of
                Nothing -> getField pkgId "haddock-html"
                Just htmlTemplate -> return (substDir pkgId lbi htmlTemplate)
            return $ if null interface
                then Nothing
                else Just $ "--read-interface=" ++
                            (if null html then "" else html ++ ",") ++
                            interface
    packageFlags <- liftM catMaybes $ mapM makeReadInterface (packageDeps lbi)

    withLib pkg_descr () $ \lib -> do
        let bi = libBuildInfo lib
        inFiles <- getModulePaths lbi bi (exposedModules lib ++ otherModules bi)
        mockAll bi inFiles
        let prologName = showPkg ++ "-haddock-prolog.txt"
        writeFile prologName (description pkg_descr ++ "\n")
        let outFiles = replaceLitExts inFiles
        let haddockFile = haddockPref pkg_descr </> haddockName pkg_descr
        -- FIX: replace w/ rawSystemProgramConf?
        rawSystemProgram verbosity confHaddock
                ([outputFlag,
                  "--odir=" ++ haddockPref pkg_descr,
                  "--title=" ++ showPkg ++ ": " ++ synopsis pkg_descr,
                  "--package=" ++ showPkg,
                  "--dump-interface=" ++ haddockFile,
                  "--prologue=" ++ prologName]
                 ++ ghcpkgFlags
                 ++ allowMissingHtmlFlags
                 ++ packageFlags
                 ++ programArgs confHaddock
                 ++ (if verbosity >= deafening then ["--verbose"] else [])
                 ++ outFiles
                 ++ map ("--hide=" ++) (otherModules bi)
                )
        removeFile prologName
    withExe pkg_descr $ \exe -> do
        let bi = buildInfo exe
            exeTargetDir = haddockPref pkg_descr </> exeName exe
        createDirectoryIfMissingVerbose verbosity True exeTargetDir
        inFiles' <- getModulePaths lbi bi (otherModules bi)
        srcMainPath <- findFile (hsSourceDirs bi) (modulePath exe)
        let inFiles = srcMainPath : inFiles'
        mockAll bi inFiles
        let outFiles = replaceLitExts inFiles
        rawSystemProgram verbosity confHaddock
                ([outputFlag,
                  "--odir=" ++ exeTargetDir,
                  "--title=" ++ exeName exe]
                 ++ ghcpkgFlags
                 ++ allowMissingHtmlFlags
                 ++ packageFlags
                 ++ programArgs confHaddock
                 ++ (if verbosity >= deafening then ["--verbose"] else [])
                 ++ outFiles
                )

    removeDirectoryRecursive tmpDir
  where
        mockPP inputArgs bi pref file
            = do let (filePref, fileName) = splitFileName file
                 let targetDir  = pref </> filePref
                 let targetFile = targetDir </> fileName
                 let (targetFileNoext, targetFileExt) = splitExtension targetFile
                 createDirectoryIfMissingVerbose verbosity True targetDir
                 if (needsCpp pkg_descr)
                    then runSimplePreProcessor (ppCpp' inputArgs bi lbi)
                           file targetFile verbosity
                    else copyFile file targetFile
                 when (targetFileExt == ".lhs") $ do
                       runSimplePreProcessor ppUnlit
                         targetFile (targetFileNoext <.> "hs") verbosity
                       return ()
        needsCpp :: PackageDescription -> Bool
        needsCpp p = case library p of
          Nothing  -> False
          Just lib -> CPP `elem` extensions (libBuildInfo lib)


-- --------------------------------------------------------------------------
-- Programmatica support

pfe :: PackageDescription -> LocalBuildInfo -> UserHooks -> PFEFlags -> IO ()
pfe pkg_descr _lbi hooks (PFEFlags verbosity) = do
    let pps = allSuffixHandlers hooks
    unless (hasLibs pkg_descr) $
        die "no libraries found in this project"
    withLib pkg_descr () $ \lib -> do
        lbi <- getPersistBuildConfig
        let bi = libBuildInfo lib
        let mods = exposedModules lib ++ otherModules (libBuildInfo lib)
        preprocessSources pkg_descr lbi verbosity pps
        inFiles <- getModulePaths lbi bi mods
        let verbFlags = if verbosity >= deafening then ["-v"] else []
        rawSystemProgramConf verbosity
                             (programName pfesetupProgram)
                             (withPrograms lbi)
                             ("noplogic" : "cpp" : verbFlags ++ inFiles)
        return ()


-- --------------------------------------------------------------------------
-- Cleaning

clean :: PackageDescription -> Maybe LocalBuildInfo -> UserHooks -> CleanFlags -> IO ()
clean pkg_descr maybeLbi hooks (CleanFlags saveConfigure _verbosity) = do
    let pps = allSuffixHandlers hooks
    putStrLn "cleaning..."
    try $ removeDirectoryRecursive (distPref </> "doc")
    try $ removeFile installedPkgConfigFile
    try $ unless saveConfigure (removeFile localBuildInfoFile)
    try $ removeFile regScriptLocation
    try $ removeFile unregScriptLocation
    removePreprocessedPackage pkg_descr currentDir (ppSuffixes pps)
    mapM_ removeFileOrDirectory (extraTmpFiles pkg_descr)

    case maybeLbi of
      Nothing  -> return ()
      Just lbi -> do
        try $ removeDirectoryRecursive (buildDir lbi)
        case compilerFlavor (compiler lbi) of
          GHC -> cleanGHCExtras lbi
          JHC -> cleanJHCExtras lbi
          _   -> return ()
  where
        cleanGHCExtras _ = do
            -- remove source stubs for library
            withLib pkg_descr () $ \ Library{libBuildInfo=bi} ->
                removeGHCModuleStubs bi (libModules pkg_descr)
            -- remove source stubs for executables
            withExe pkg_descr $ \ Executable{modulePath=exeSrcName
                                            ,buildInfo=bi} -> do
                removeGHCModuleStubs bi (exeModules pkg_descr)
                let (startN, _) = splitExtension exeSrcName
                try $ removeFile (startN ++ "_stub.h")
                try $ removeFile (startN ++ "_stub.c")
        removeGHCModuleStubs :: BuildInfo -> [String] -> IO ()
        removeGHCModuleStubs (BuildInfo{hsSourceDirs=dirs}) mods = do
            s <- mapM (\x -> moduleToFilePath dirs (x ++"_stub") ["h", "c"]) mods
            mapM_ removeFile (concat s)
        -- JHC FIXME remove exe-sources
        cleanJHCExtras lbi = do
            try $ removeFile (buildDir lbi </> "jhc-pkg.conf")
            removePreprocessedPackage pkg_descr currentDir ["ho"]
        removeFileOrDirectory :: FilePath -> IO ()
        removeFileOrDirectory fname = do
            isDir <- doesDirectoryExist fname
            isFile <- doesFileExist fname
            if isDir then removeDirectoryRecursive fname
              else if isFile then removeFile fname
              else return ()

-- --------------------------------------------------------------------------
-- Default hooks

no_extra_flags :: [String] -> IO ()
no_extra_flags [] = return ()
no_extra_flags extra_flags =
 die $ concat
     $ intersperse "\n" ("Unrecognised flags:" : map (' ' :) extra_flags)

scratchDirOpt :: OptDescr (LocalBuildInfo -> LocalBuildInfo)
scratchDirOpt = Option "b" ["scratchdir"] (reqDirArg setScratchDir)
		"directory to receive the built package [dist/scratch]"
  where setScratchDir dir lbi = lbi { scratchDir = dir }

-- |Empty 'UserHooks' which do nothing.
emptyUserHooks :: UserHooks
emptyUserHooks
    = UserHooks
      {
       runTests  = ru,
       readDesc  = return Nothing,
       hookedPreProcessors = [],
       hookedPrograms      = [],
       preConf   = rn,
       confHook  = (\_ _ -> return (error "No local build info generated during configure. Over-ride empty configure hook.")),
       postConf  = ru,
       preBuild  = rn,
       buildHook = ru,
       postBuild = ru,
       preMakefile = rn,
       makefileHook = ru,
       postMakefile = ru,
       preClean  = rn,
       cleanHook = ru,
       postClean = ru,
       preCopy   = rn,
       copyHook  = ru,
       postCopy  = ru,
       preInst   = rn,
       instHook  = ru,
       postInst  = ru,
       preSDist  = rn,
       sDistHook = ru,
       postSDist = ru,
       preReg    = rn,
       regHook   = ru,
       postReg   = ru,
       preUnreg  = rn,
       unregHook = ru,
       postUnreg = ru,
       prePFE    = rn,
       pfeHook   = ru,
       postPFE   = ru,
       preHaddock  = rn,
       haddockHook = ru,
       postHaddock = ru
      }
    where rn  args _   = no_extra_flags args >> return emptyHookedBuildInfo
          ru _ _ _ _ = return ()

-- | Hooks that correspond to a plain instantiation of the 
-- "simple" build system
simpleUserHooks :: UserHooks
simpleUserHooks = 
    emptyUserHooks {
       confHook  = configure,
       buildHook = defaultBuildHook,
       makefileHook = defaultMakefileHook,
       copyHook  = \desc lbi _ f -> install desc lbi f, -- has correct 'copy' behavior with params
       instHook  = defaultInstallHook,
       sDistHook = \p l h f -> sdist p l f srcPref distPref (allSuffixHandlers h),
       pfeHook   = pfe,
       cleanHook = clean,
       haddockHook = haddock,
       regHook   = defaultRegHook,
       unregHook = \p l _ f -> unregister p l f
      }

-- | Basic autoconf 'UserHooks':
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
defaultUserHooks = autoconfUserHooks

autoconfUserHooks :: UserHooks
autoconfUserHooks
    = simpleUserHooks
      {
       postConf  = defaultPostConf,
       preBuild  = readHook buildVerbose,
       preMakefile = readHook makefileVerbose,
       preClean  = readHook cleanVerbose,
       preCopy   = readHook copyVerbose,
       preInst   = readHook installVerbose,
       preHaddock  = readHook haddockVerbose,
       preReg    = readHook regVerbose,
       preUnreg  = readHook regVerbose
      }
    where defaultPostConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
          defaultPostConf args flags _ _
              = do let verbosity = configVerbose flags
                       args' = configureArgs flags ++ args
                   confExists <- doesFileExist "configure"
                   if confExists then
                       rawSystemPathExit verbosity "sh" ("configure" : args')
                     else
                       no_extra_flags args

          readHook :: (a -> Verbosity) -> Args -> a -> IO HookedBuildInfo
          readHook get_verbosity a flags = do
              no_extra_flags a
              maybe_infoFile <- defaultHookedPackageDesc
              case maybe_infoFile of
                  Nothing       -> return emptyHookedBuildInfo
                  Just infoFile -> do
                      let verbosity = get_verbosity flags
                      when (verbosity >= normal) $
                          putStrLn $ "Reading parameters from " ++ infoFile
                      readHookedBuildInfo verbosity infoFile

defaultInstallHook :: PackageDescription -> LocalBuildInfo
                   -> UserHooks -> InstallFlags -> IO ()
defaultInstallHook pkg_descr localbuildinfo _ (InstallFlags uInstFlag verbosity) = do
  install pkg_descr localbuildinfo (CopyFlags NoCopyDest verbosity)
  when (hasLibs pkg_descr) $
      register pkg_descr localbuildinfo
           emptyRegisterFlags{ regUser=uInstFlag, regVerbose=verbosity }

defaultBuildHook :: PackageDescription -> LocalBuildInfo
	-> UserHooks -> BuildFlags -> IO ()
defaultBuildHook pkg_descr localbuildinfo hooks flags = do
  build pkg_descr localbuildinfo flags (allSuffixHandlers hooks)
  when (hasLibs pkg_descr) $
      writeInstalledConfig pkg_descr localbuildinfo False

defaultMakefileHook :: PackageDescription -> LocalBuildInfo
	-> UserHooks -> MakefileFlags -> IO ()
defaultMakefileHook pkg_descr localbuildinfo hooks flags = do
  makefile pkg_descr localbuildinfo flags (allSuffixHandlers hooks)
  when (hasLibs pkg_descr) $
      writeInstalledConfig pkg_descr localbuildinfo False

defaultRegHook :: PackageDescription -> LocalBuildInfo
	-> UserHooks -> RegisterFlags -> IO ()
defaultRegHook pkg_descr localbuildinfo _ flags =
    if hasLibs pkg_descr
    then register pkg_descr localbuildinfo flags
    else setupMessage (regVerbose flags)
                      "Package contains no library to register:"
                      pkg_descr

-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

-- |Output warnings and errors. Exit if any errors.
errorOut :: Verbosity -- ^Verbosity
         -> [String]  -- ^Warnings
         -> [String]  -- ^errors
         -> IO ()
errorOut verbosity warnings errors = do
  mapM_ (warn verbosity) warnings
  when (not (null errors)) $ do
    pname <- getProgName
    mapM (hPutStrLn stderr . ((pname ++ ": Error: ") ++)) errors
    exitWith (ExitFailure 1)

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
simpleHunitTests :: [Test]
simpleHunitTests = []
#endif
