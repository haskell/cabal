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
	module Distribution.Simple.Compiler,
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
import Distribution.Simple.Compiler
import Distribution.Simple.UserHooks
import Distribution.Package --must not specify imports, since we're exporting moule.
import Distribution.PackageDescription
import Distribution.Simple.Program(ProgramConfiguration,
                            defaultProgramConfiguration, addKnownProgram,
                            pfesetupProgram, rawSystemProgramConf)
import Distribution.Simple.PreProcess (knownSuffixHandlers,
                                removePreprocessedPackage,
                                preprocessSources, PPSuffixHandler)
import Distribution.Simple.Setup

import Distribution.Simple.Build	( build, makefile )
import Distribution.Simple.SrcDist	( sdist )
import Distribution.Simple.Register	( register, unregister,
                                          writeInstalledConfig,
                                          removeRegScripts
                                        )

import Distribution.Simple.Configure(getPersistBuildConfig, 
                                     maybeGetPersistBuildConfig,
                                     checkPersistBuildConfig,
                                     configure, writePersistBuildConfig)

import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..), distPref, srcPref)
import Distribution.Simple.Install (install)
import Distribution.Simple.Haddock (haddock, hscolour)
import Distribution.Simple.Utils (die, currentDir, moduleToFilePath,
                                  defaultPackageDesc, defaultHookedPackageDesc)

import Distribution.Simple.Utils (rawSystemPathExit, notice, info)
import Distribution.Verbosity
import Language.Haskell.Extension
-- Base
import System.Environment(getArgs)
import System.Directory(removeFile, doesFileExist, doesDirectoryExist)

import Distribution.License
import Control.Monad   (when, unless)
import Data.List       (intersperse, unionBy)
import System.IO.Error (try)
import Distribution.GetOpt

import Distribution.Compat.Directory(removeDirectoryRecursive)
import System.FilePath((</>))

#ifdef DEBUG
import Test.HUnit (Test)
import Distribution.Version hiding (hunitTests)
#else
import Distribution.Version
#endif

-- | A simple implementation of @main@ for a Cabal setup script.
-- It reads the package description file using IO, and performs the
-- action specified on the command line.
defaultMain :: IO ()
defaultMain = getArgs >>= defaultMain__ simpleUserHooks

-- | A version of 'defaultMain' that is passed the command line
-- arguments, rather than getting them from the environment.
defaultMainArgs :: [String] -> IO ()
defaultMainArgs = defaultMain__ simpleUserHooks

-- | A customizable version of 'defaultMain'.
defaultMainWithHooks :: UserHooks -> IO ()
defaultMainWithHooks hooks = getArgs >>= defaultMain__ hooks

-- | A customizable version of 'defaultMain' that also takes the command
-- line arguments.
defaultMainWithHooksArgs :: UserHooks -> [String] -> IO ()
defaultMainWithHooksArgs = defaultMain__

-- | Like 'defaultMain', but accepts the package description as input
-- rather than using IO to read it.
defaultMainNoRead :: PackageDescription -> IO ()
defaultMainNoRead pkg_descr =
  getArgs >>=
  defaultMain__ simpleUserHooks { readDesc = return (Just pkg_descr) }

defaultMain__ :: UserHooks
              -> [String]
              -> IO ()
defaultMain__ hooks args = do
   let prog_conf = allPrograms hooks
   (action, args') <- parseGlobalArgs prog_conf args
   -- let get_pkg_descr verbosity = case mdescr of
--         Just pkg_descr -> return pkg_descr
--         Nothing -> do
--           maybeDesc <- readDesc hooks
--           case maybeDesc of
-- 	    Nothing -> defaultPkgDescr
-- 	    Just p  -> return p
--          where
--            defaultPkgDescr = do
--              -- FIXME: add compat mode or flag to enable configs
--              pkg_descr_file <- defaultPackageDesc verbosity 
--              readPackageDescription verbosity pkg_descr_file
   defaultMainWorker action args' hooks prog_conf

-- | Combine the programs in the given hooks with the programs built
-- into cabal.
allPrograms :: UserHooks
            -> ProgramConfiguration -- combine defaults w/ user programs
allPrograms h = foldl (flip addKnownProgram) 
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
defaultMainWorker :: Action
                  -> [String]
                  -> UserHooks
                  -> ProgramConfiguration
                  -> IO ()
defaultMainWorker action all_args hooks prog_conf
    = do case action of
            ConfigCmd flags -> do
                (flags', _, args) <-
			parseConfigureArgs prog_conf flags all_args []
                pbi <- preConf hooks args flags'

                (mb_pd_file, pkg_descr0) <- confPkgDescr flags'

                --    get_pkg_descr (configVerbose flags')
                --let pkg_descr = updatePackageDescription pbi pkg_descr0
                let epkg_descr = (pkg_descr0, pbi)

                --(warns, ers) <- sanityCheckPackage pkg_descr
                --errorOut (configVerbose flags') warns ers

		localbuildinfo0 <- confHook hooks epkg_descr flags'

                -- remember the .cabal filename if we know it
                let localbuildinfo = localbuildinfo0{ pkgDescrFile = mb_pd_file }
                writePersistBuildConfig localbuildinfo
                
		let pkg_descr = localPkgDescr localbuildinfo
                postConf hooks args flags' pkg_descr localbuildinfo
              where
                confPkgDescr :: ConfigFlags
                             -> IO (Maybe FilePath,
                                    Either GenericPackageDescription
                                           PackageDescription)
                confPkgDescr cfgflags = do
                  mdescr <- readDesc hooks
                  case mdescr of
                    Just descr -> return (Nothing, Right descr)
                    Nothing -> do
                      pdfile <- defaultPackageDesc (configVerbose cfgflags)
                      ppd <- readPackageDescription (configVerbose cfgflags) pdfile
                      return (Just pdfile, Left ppd)

            BuildCmd -> do
                lbi <- getBuildConfigIfUpToDate
                res@(flags, _, _) <-
                  parseBuildArgs prog_conf
                                 (emptyBuildFlags (withPrograms lbi)) all_args []
                command (\_ _ -> return res) buildVerbose
                        preBuild buildHook postBuild
                        (return lbi { withPrograms = buildPrograms flags })

            MakefileCmd ->
                command (parseMakefileArgs emptyMakefileFlags) makefileVerbose
                        preMakefile makefileHook postMakefile
                        getBuildConfigIfUpToDate

            HscolourCmd ->
                command (parseHscolourArgs emptyHscolourFlags) hscolourVerbose
                        preHscolour hscolourHook postHscolour
                        getBuildConfigIfUpToDate
        
            HaddockCmd -> 
                command (parseHaddockArgs emptyHaddockFlags) haddockVerbose
                        preHaddock haddockHook postHaddock
                        getBuildConfigIfUpToDate

            ProgramaticaCmd -> do
                command parseProgramaticaArgs pfeVerbose
                        prePFE pfeHook postPFE
                        getBuildConfigIfUpToDate

            CleanCmd -> do
                (flags, _, args) <- parseCleanArgs emptyCleanFlags all_args []

                pbi <- preClean hooks args flags

                mlbi <- maybeGetPersistBuildConfig
                pdfile <- defaultPackageDesc (cleanVerbose flags)
                ppd <- readPackageDescription (cleanVerbose flags) pdfile
                let pkg_descr0 = flattenPackageDescription ppd
                let pkg_descr = updatePackageDescription pbi pkg_descr0

                cleanHook hooks pkg_descr mlbi hooks flags
                postClean hooks args flags pkg_descr mlbi

            CopyCmd mprefix -> do
                command (parseCopyArgs (emptyCopyFlags mprefix)) copyVerbose
                        preCopy copyHook postCopy
                        getBuildConfigIfUpToDate

            InstallCmd -> do
                command (parseInstallArgs emptyInstallFlags) installVerbose
                        preInst instHook postInst
                        getBuildConfigIfUpToDate

            SDistCmd -> do
                (flags, _, args) <- parseSDistArgs all_args []

                pbi <- preSDist hooks args flags

                mlbi <- maybeGetPersistBuildConfig
                pdfile <- defaultPackageDesc (sDistVerbose flags)
                ppd <- readPackageDescription (sDistVerbose flags) pdfile
                let pkg_descr0 = flattenPackageDescription ppd
                let pkg_descr = updatePackageDescription pbi pkg_descr0

                sDistHook hooks pkg_descr mlbi hooks flags
                postSDist hooks args flags pkg_descr mlbi

            TestCmd -> do
                (_verbosity,_, args) <- parseTestArgs all_args []
                localbuildinfo <- getBuildConfigIfUpToDate
                let pkg_descr = localPkgDescr localbuildinfo
                runTests hooks args False pkg_descr localbuildinfo

            RegisterCmd  -> do
                command (parseRegisterArgs emptyRegisterFlags) regVerbose
                        preReg regHook postReg
                        getBuildConfigIfUpToDate

            UnregisterCmd -> do
                command (parseUnregisterArgs emptyRegisterFlags) regVerbose
                        preUnreg unregHook postUnreg
                        getBuildConfigIfUpToDate

            HelpCmd -> return () -- this is handled elsewhere
        where
        command parse_args _get_verbosity
                pre_hook cmd_hook post_hook
                get_build_config = do
           (flags, _, args) <- parse_args all_args []
           pbi <- pre_hook hooks args flags
           localbuildinfo <- get_build_config
           let pkg_descr0 = localPkgDescr localbuildinfo
           --pkg_descr0 <- get_pkg_descr (get_verbose flags)
           let pkg_descr = updatePackageDescription pbi pkg_descr0
           -- XXX: should we write the modified package descr back to the
           -- localbuildinfo?
           cmd_hook hooks pkg_descr localbuildinfo hooks flags
           post_hook hooks args flags pkg_descr localbuildinfo


--TODO: where to put this? it's duplicated in .Haddock too
getModulePaths :: LocalBuildInfo -> BuildInfo -> [String] -> IO [FilePath]
getModulePaths lbi bi =
   fmap concat .
      mapM (flip (moduleToFilePath (buildDir lbi : hsSourceDirs bi)) ["hs", "lhs"])

getBuildConfigIfUpToDate :: IO LocalBuildInfo
getBuildConfigIfUpToDate = do
   lbi <- getPersistBuildConfig
   case pkgDescrFile lbi of
     Nothing -> return ()
     Just pkg_descr_file -> checkPersistBuildConfig pkg_descr_file
   return lbi

-- --------------------------------------------------------------------------
-- Programmatica support

pfe :: PackageDescription -> [PPSuffixHandler] -> PFEFlags -> IO ()
pfe pkg_descr pps (PFEFlags verbosity) = do
    unless (hasLibs pkg_descr) $
        die "no libraries found in this project"
    withLib pkg_descr () $ \lib -> do
        lbi <- getPersistBuildConfig
        let bi = libBuildInfo lib
        let mods = exposedModules lib ++ otherModules (libBuildInfo lib)
        preprocessSources pkg_descr lbi False verbosity pps
        inFiles <- getModulePaths lbi bi mods
        let verbFlags = if verbosity >= deafening then ["-v"] else []
        rawSystemProgramConf verbosity pfesetupProgram (withPrograms lbi)
                             ("noplogic" : "cpp" : verbFlags ++ inFiles)


-- --------------------------------------------------------------------------
-- Cleaning

clean :: PackageDescription -> Maybe LocalBuildInfo -> CleanFlags -> IO ()
clean pkg_descr maybeLbi (CleanFlags saveConfigure verbosity) = do
    notice verbosity "cleaning..."

    maybeConfig <- if saveConfigure then maybeGetPersistBuildConfig
                                    else return Nothing

    -- remove the whole dist/ directory rather than tracking exactly what files
    -- we created in there.
    try $ removeDirectoryRecursive distPref

    -- these live in the top level dir so must be removed separately
    removeRegScripts

    -- Any extra files the user wants to remove
    mapM_ removeFileOrDirectory (extraTmpFiles pkg_descr)

    -- FIXME: put all JHC's generated files under dist/ so they get cleaned
    case maybeLbi of
      Nothing  -> return ()
      Just lbi -> do
        case compilerFlavor (compiler lbi) of
          JHC -> cleanJHCExtras lbi
          _   -> return ()

    -- If the user wanted to save the config, write it back
    maybe (return ()) writePersistBuildConfig maybeConfig

  where
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

-- | Hooks that correspond to a plain instantiation of the 
-- \"simple\" build system
simpleUserHooks :: UserHooks
simpleUserHooks = 
    emptyUserHooks {
       confHook  = configure,
       buildHook = defaultBuildHook,
       makefileHook = defaultMakefileHook,
       copyHook  = \desc lbi _ f -> install desc lbi f, -- has correct 'copy' behavior with params
       instHook  = defaultInstallHook,
       sDistHook = \p l h f -> sdist p l f srcPref distPref (allSuffixHandlers h),
       pfeHook   = \p _ h f -> pfe   p (allSuffixHandlers h) f,
       cleanHook = \p l _ f -> clean p l f,
       hscolourHook = \p l h f -> hscolour p l (allSuffixHandlers h) f,
       haddockHook  = \p l h f -> haddock  p l (allSuffixHandlers h) f,
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
       preHscolour = readHook hscolourVerbose,
       preHaddock  = readHook haddockVerbose,
       preReg    = readHook regVerbose,
       preUnreg  = readHook regVerbose
      }
    where defaultPostConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
          defaultPostConf args flags _ _
              = do let verbosity = configVerbose flags
                   no_extra_flags args
                   confExists <- doesFileExist "configure"
                   when confExists $
                       rawSystemPathExit verbosity "sh" $
                           "configure" : configureArgs flags

          readHook :: (a -> Verbosity) -> Args -> a -> IO HookedBuildInfo
          readHook get_verbosity a flags = do
              no_extra_flags a
              maybe_infoFile <- defaultHookedPackageDesc
              case maybe_infoFile of
                  Nothing       -> return emptyHookedBuildInfo
                  Just infoFile -> do
                      let verbosity = get_verbosity flags
                      info verbosity $ "Reading parameters from " ++ infoFile
                      readHookedBuildInfo verbosity infoFile

defaultInstallHook :: PackageDescription -> LocalBuildInfo
                   -> UserHooks -> InstallFlags -> IO ()
defaultInstallHook pkg_descr localbuildinfo _ (InstallFlags uInstFlag verbosity) = do
  install pkg_descr localbuildinfo (CopyFlags NoCopyDest verbosity)
  when (hasLibs pkg_descr) $
      register pkg_descr localbuildinfo
           emptyRegisterFlags{ regPackageDB=uInstFlag, regVerbose=verbosity }

defaultBuildHook :: PackageDescription -> LocalBuildInfo
	-> UserHooks -> BuildFlags -> IO ()
defaultBuildHook pkg_descr localbuildinfo hooks flags = do
  build pkg_descr localbuildinfo flags (allSuffixHandlers hooks)
  when (hasLibs pkg_descr) $
      writeInstalledConfig pkg_descr localbuildinfo False Nothing

defaultMakefileHook :: PackageDescription -> LocalBuildInfo
	-> UserHooks -> MakefileFlags -> IO ()
defaultMakefileHook pkg_descr localbuildinfo hooks flags = do
  makefile pkg_descr localbuildinfo flags (allSuffixHandlers hooks)
  when (hasLibs pkg_descr) $
      writeInstalledConfig pkg_descr localbuildinfo False Nothing

defaultRegHook :: PackageDescription -> LocalBuildInfo
	-> UserHooks -> RegisterFlags -> IO ()
defaultRegHook pkg_descr localbuildinfo _ flags =
    if hasLibs pkg_descr
    then register pkg_descr localbuildinfo flags
    else setupMessage (regVerbose flags)
                      "Package contains no library to register:"
                      pkg_descr

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
simpleHunitTests :: [Test]
simpleHunitTests = []
#endif
