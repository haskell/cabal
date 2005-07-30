-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
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
	defaultMain, defaultMainNoRead,
        -- * Customization
        UserHooks(..), Args,
        defaultMainWithHooks, defaultUserHooks, emptyUserHooks,
        defaultHookedPackageDesc,
#ifdef DEBUG        
        simpleHunitTests
#endif
  ) where

-- local
import Distribution.Compiler
import Distribution.Package --must not specify imports, since we're exporting moule.
import Distribution.PackageDescription
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

import Distribution.Simple.Configure(LocalBuildInfo(..), getPersistBuildConfig, findProgram,
				     configure, writePersistBuildConfig, localBuildInfoFile)
import Distribution.Simple.Install(install)
import Distribution.Simple.Utils (die, currentDir, rawSystemVerbose,
                                  defaultPackageDesc, defaultHookedPackageDesc,
                                  moduleToFilePath)
import Language.Haskell.Extension
-- Base
import System.Environment(getArgs)
import System.Exit(ExitCode(..), exitWith)
import System.Directory(removeFile, doesFileExist)

import Distribution.License
import Control.Monad(when, unless)
import Data.List	( intersperse, unionBy )
import Data.Maybe       ( isNothing, fromJust )
import System.IO.Error (try)
import Distribution.GetOpt

import Distribution.Compat.Directory(createDirectoryIfMissing,removeDirectoryRecursive, copyFile)
import Distribution.Compat.FilePath(joinFileName, joinPaths, splitFileName, joinFileExt,
                                    splitFileExt, changeFileExt)

#ifdef DEBUG
import HUnit (Test)
import Distribution.Version hiding (hunitTests)
#else
import Distribution.Version
#endif


type Args = [String]

-- | Hooks allow authors to add specific functionality before and after
-- a command is run, and also to specify additional preprocessors.
data UserHooks = UserHooks
    {
     runTests :: Args -> Bool -> LocalBuildInfo -> IO ExitCode, -- ^Used for @.\/setup test@
     readDesc :: IO (Maybe PackageDescription), -- ^Read the description file
     hookedPreProcessors :: [ PPSuffixHandler ],
        -- ^Custom preprocessors in addition to and overriding 'knownSuffixHandlers'.

      -- |Hook to run before configure command
     preConf  :: Args -> ConfigFlags -> IO HookedBuildInfo,
      -- |Hook to run after configure command
     postConf :: Args -> ConfigFlags -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before build command.  Second arg indicates verbosity level.
     preBuild  :: Args -> Int -> IO HookedBuildInfo,
      -- |Hook to run after build command.  Second arg indicates verbosity level.
     postBuild :: Args -> Int -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before clean command.  Second arg indicates verbosity level.
     preClean  :: Args -> Int -> IO HookedBuildInfo,
      -- |Hook to run after clean command.  Second arg indicates verbosity level.
     postClean :: Args -> Int -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before copy command
     preCopy  :: Args -> CopyFlags -> IO HookedBuildInfo,
      -- |Hook to run after copy command
     postCopy :: Args -> CopyFlags -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before install command
     preInst  :: Args -> InstallFlags -> IO HookedBuildInfo,
      -- |Hook to run after install command.  postInst should be run
      -- on the target, not on the build machine.
     postInst :: Args -> InstallFlags -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before sdist command.  Second arg indicates verbosity level.
     preSDist  :: Args -> Int -> IO HookedBuildInfo,
      -- |Hook to run after sdist command.  Second arg indicates verbosity level.
     postSDist :: Args -> Int -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before register command
     preReg  :: Args -> RegisterFlags -> IO HookedBuildInfo,
      -- |Hook to run after register command
     postReg :: Args -> RegisterFlags -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before unregister command
     preUnreg  :: Args -> RegisterFlags -> IO HookedBuildInfo,
      -- |Hook to run after unregister command
     postUnreg :: Args -> RegisterFlags -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before haddock command.  Second arg indicates verbosity level.
     preHaddock  :: Args -> Int -> IO HookedBuildInfo,
      -- |Hook to run after haddock command.  Second arg indicates verbosity level.
     postHaddock :: Args -> Int -> LocalBuildInfo -> IO ExitCode,

      -- |Hook to run before pfe command.  Second arg indicates verbosity level.
     prePFE  :: Args -> Int -> IO HookedBuildInfo,
      -- |Hook to run after  pfe command.  Second arg indicates verbosity level.
     postPFE :: Args -> Int -> LocalBuildInfo -> IO ExitCode

    }

-- |A simple implementation of @main@ for a Cabal setup script.
-- It reads the package description file using IO, and performs the
-- action specified on the command line.
defaultMain :: IO ()
defaultMain = do args <- getArgs
                 (action, args) <- parseGlobalArgs args
                 pkg_descr_file <- defaultPackageDesc
                 pkg_descr <- readPackageDescription pkg_descr_file
                 defaultMainWorker pkg_descr action args Nothing
                 return ()

-- | A customizable version of 'defaultMain'.
defaultMainWithHooks :: UserHooks -> IO ()
defaultMainWithHooks hooks
    = do args <- getArgs
         (action, args) <- parseGlobalArgs args
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
         (action, args) <- parseGlobalArgs args
         defaultMainWorker pkg_descr action args Nothing
         return ()

-- |Helper function for /defaultMain/ and /defaultMainNoRead/
defaultMainWorker :: PackageDescription
                  -> Action
                  -> [String] -- ^args1
                  -> Maybe UserHooks
                  -> IO ExitCode
defaultMainWorker pkg_descr_in action args hooks
    = do let pps = maybe knownSuffixHandlers
                         (\h -> overridesPP (hookedPreProcessors h) knownSuffixHandlers)
                         hooks
         case action of
            ConfigCmd flags -> do
                (flags, optFns, args) <-
			parseConfigureArgs flags args [buildDirOpt]
                pkg_descr <- hookOrInArgs preConf args flags
                (warns, ers) <- sanityCheckPackage pkg_descr
                errorOut warns ers
		localbuildinfo <- configure pkg_descr flags
		writePersistBuildConfig (foldr id localbuildinfo optFns)
                postHook postConf args flags localbuildinfo

            BuildCmd -> do
                (flags, _, args) <- parseBuildArgs args []
                pkg_descr <- hookOrInArgs preBuild args flags
		localbuildinfo <- getPersistBuildConfig
		build pkg_descr localbuildinfo flags pps
                when (hasLibs pkg_descr) $
                    writeInstalledConfig pkg_descr localbuildinfo
                postHook postBuild args flags localbuildinfo

            HaddockCmd -> do
                (verbose, _, args) <- parseHaddockArgs args []
                pkg_descr <- hookOrInArgs preHaddock args verbose
		localbuildinfo <- getPersistBuildConfig
                haddock pkg_descr localbuildinfo verbose pps
                postHook postHaddock args verbose localbuildinfo

            ProgramaticaCmd -> do
                (verbose, _, args) <- parseProgramaticaArgs args []
                pkg_descr <- hookOrInArgs prePFE args verbose
                localbuildinfo <- getPersistBuildConfig
                pfe pkg_descr localbuildinfo verbose pps
                postHook postPFE args verbose localbuildinfo

            CleanCmd -> do
                (verbose,_, args) <- parseCleanArgs args []
                pkg_descr <- hookOrInArgs preClean args verbose
		localbuildinfo <- getPersistBuildConfig
                clean pkg_descr localbuildinfo verbose pps
                postHook postClean args verbose localbuildinfo

            CopyCmd mprefix -> do
                (flags, _, args) <- parseCopyArgs (mprefix,0) args []
                pkg_descr <- hookOrInArgs preCopy args flags
		localbuildinfo <- getPersistBuildConfig
		install pkg_descr localbuildinfo flags
                postHook postCopy args flags localbuildinfo

            InstallCmd uInst -> do
                (flags@(uInst, verbose), _, args) <- parseInstallArgs (uInst,0) args []
                pkg_descr <- hookOrInArgs preInst args flags
		localbuildinfo <- getPersistBuildConfig
		install pkg_descr localbuildinfo (Nothing, verbose)
                when (hasLibs pkg_descr)
                         (register pkg_descr localbuildinfo (uInst, False, verbose))
                postHook postInst args flags localbuildinfo

            SDistCmd -> do
                let srcPref   = distPref `joinFileName` "src"
                ((snapshot,verbose),_, args) <- parseSDistArgs args []
                pkg_descr <- hookOrInArgs preSDist args verbose
                localbuildinfo <- getPersistBuildConfig
                sdist srcPref distPref verbose snapshot pps pkg_descr
                postHook postSDist args verbose localbuildinfo

            TestCmd -> do
                (verbose,_, args) <- parseTestArgs args []
                case hooks of
                 Nothing -> return ExitSuccess
                 Just h  -> do localbuildinfo <- getPersistBuildConfig
                               out <- (runTests h) args False localbuildinfo
                               when (isFailure out) (exitWith out)
                               return out

            RegisterCmd uInst genScript -> do
                (flags, _, args) <- parseRegisterArgs (uInst, genScript, 0) args []
                pkg_descr <- hookOrInArgs preReg args flags
		localbuildinfo <- getPersistBuildConfig
		if hasLibs pkg_descr
                   then register pkg_descr localbuildinfo flags
                   else die "Package contains no library to register"
                postHook postReg args flags localbuildinfo

            UnregisterCmd uInst genScript -> do
                (flags,_, args) <- parseUnregisterArgs (uInst,genScript, 0) args []
                pkg_descr <- hookOrInArgs preUnreg args flags
		localbuildinfo <- getPersistBuildConfig
		unregister pkg_descr localbuildinfo flags
                postHook postUnreg args flags localbuildinfo

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
        postHook f args flags localbuildinfo
                 = case hooks of
                    Nothing -> return ExitSuccess
                    Just h  -> f h args flags localbuildinfo
        isFailure :: ExitCode -> Bool
        isFailure (ExitFailure _) = True
        isFailure _               = False

        overridesPP :: [PPSuffixHandler] -> [PPSuffixHandler] -> [PPSuffixHandler]
        overridesPP = unionBy (\x y -> fst x == fst y)
-- (filter (\x -> notElem x overriders) overridden) ++ overriders

distPref :: FilePath
distPref = "dist"

haddock :: PackageDescription -> LocalBuildInfo -> Int -> [PPSuffixHandler] -> IO ()
haddock pkg_descr lbi verbose pps =
    withLib pkg_descr () $ \lib -> do
        mHaddock <- findProgram "haddock" (withHaddock lbi)
        when (isNothing mHaddock) (die "haddock command not found")
        let bi = libBuildInfo lib
        let targetDir = joinPaths distPref (joinPaths "doc" "html")
        let tmpDir = joinPaths (buildDir lbi) "tmp"
        createDirectoryIfMissing True tmpDir
        createDirectoryIfMissing True targetDir
        preprocessSources pkg_descr lbi verbose pps
        inFiles <- sequence [moduleToFilePath (hsSourceDirs bi) m ["hs", "lhs"]
                               | m <- exposedModules lib] >>= return . concat
        mapM_ (mockPP ["-D__HADDOCK__"] pkg_descr bi lbi tmpDir verbose) inFiles
        let showPkg = showPackageId (package pkg_descr)
        let prologName = showPkg ++ "-haddock-prolog.txt"
        writeFile prologName ((description pkg_descr) ++ "\n")
        setupMessage "Running Haddock for" pkg_descr
        let outFiles = map (joinFileName tmpDir)
                       (map ((flip changeFileExt) "hs") inFiles)
        code <- rawSystemVerbose verbose (fromJust mHaddock)
                (["-h",
                  "-o", targetDir,
                  "-t", showPkg,
                  "-p", prologName]
                ++ (if verbose > 4 then ["-v"] else [])
                ++ outFiles
                )
        removeDirectoryRecursive tmpDir
        removeFile prologName
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
        needsCpp p | not (hasLibs p) = False
                   | otherwise = any (== CPP) (extensions $ libBuildInfo $ fromJust $ library p)

pfe :: PackageDescription -> LocalBuildInfo -> Int -> [PPSuffixHandler] -> IO ()
pfe pkg_descr _lbi verbose pps = do
    unless (hasLibs pkg_descr) $
        die "no libraries found in this project"
    withLib pkg_descr () $ \lib -> do
        lbi <- getPersistBuildConfig
        mPfe <- findProgram "pfesetup" Nothing
        when (isNothing mPfe) (die "pfe command not found")
        putStrLn $ "using : " ++ fromJust mPfe
        let bi = libBuildInfo lib
        let mods = exposedModules lib ++ otherModules (libBuildInfo lib)
        preprocessSources pkg_descr lbi verbose pps
        inFiles <- sequence [moduleToFilePath (hsSourceDirs bi) m ["hs", "lhs"]
                                | m <- mods] >>= return . concat
        rawSystemVerbose verbose (fromJust mPfe)
                ("noplogic":"cpp": (if verbose > 4 then ["-v"] else [])
                ++ inFiles)
        return ()

clean :: PackageDescription -> LocalBuildInfo -> Int -> [PPSuffixHandler] -> IO ()
clean pkg_descr lbi verbose pps = do
    putStrLn "cleaning..."
    let buildPref = buildDir lbi
    try $ removeDirectoryRecursive buildPref
    try $ removeDirectoryRecursive (joinPaths distPref "doc")
    try $ removeFile installedPkgConfigFile
    try $ removeFile localBuildInfoFile
    try $ removeFile regScriptLocation
    try $ removeFile unregScriptLocation
    removePreprocessedPackage pkg_descr currentDir (ppSuffixes pps)
    case compilerFlavor (compiler lbi) of
      GHC -> cleanGHCExtras
      _   -> return ()
  where
        cleanGHCExtras = do
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
                s <- sequence [moduleToFilePath dirs (x ++"_stub") ["h", "c"]
                                 | x <- mods ]
                mapM_ removeFile (concat s)

no_extra_flags :: [String] -> IO ()
no_extra_flags [] = return ()
no_extra_flags extra_flags  = 
  die ("Unrecognised flags: " ++ concat (intersperse "," (extra_flags)))

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
       preConf   = rn,
       postConf  = res,
       preBuild  = rn,
       postBuild = res,
       preClean  = rn,
       postClean = res,
       preCopy   = rn,
       postCopy  = res,
       preInst   = rn,
       postInst  = res,
       preSDist  = rn,
       postSDist = res,
       preReg    = rn,
       postReg   = res,
       preUnreg  = rn,
       postUnreg = res,
       prePFE    = rn,
       postPFE   = res,
       preHaddock  = rn,
       postHaddock = res
      }
    where rn  _ _   = return emptyHookedBuildInfo
          res _ _ _ = return ExitSuccess

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
       preBuild  = readHook id,
       preClean  = readHook id,
       preCopy   = readHook snd,
       preInst   = readHook snd,
       preReg    = readHook thd3,
       preUnreg  = readHook thd3
      }
    where defaultPostConf :: Args -> ConfigFlags -> LocalBuildInfo -> IO ExitCode
          defaultPostConf args flags lbi
              = do let prefix_opt pref opts =
                           ("--prefix=" ++ pref) : opts
                   confExists <- doesFileExist "configure"
	           if confExists then do
	               rawSystemVerbose (configVerbose flags) "sh"
			   ("configure" : maybe id prefix_opt (configPrefix flags) args)
		       return ()
		     else
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

          thd3 (_,_,z) = z

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
simpleHunitTests :: [Test]
simpleHunitTests = []
#endif
