{-# OPTIONS -cpp #-}
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
-- Distribution/Simple/\* modules.  When given the parsed command-line
-- args and package information, is able to perform basic commands
-- like configure, build, install, register, etc.

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
	License(..), Version(..), VersionRange(..), 
	orLaterVersion, orEarlierVersion, betweenVersionsInclusive,
	Extension(..), Dependency(..),
	defaultMain, defaultMainNoRead, defaultMainWithHooks,
        UserHooks (..), defaultUserHooks, hookedPackageDesc,
#ifdef DEBUG        
        simpleHunitTests
#endif
  ) where

-- local
import Distribution.Package --must not specify imports, since we're exporting moule.
import Distribution.PackageDescription
import Distribution.PreProcess (knownSuffixHandlers, ppSuffixes, removePreprocessedPackage)
import Distribution.Setup

import Distribution.Simple.Build	( build )
import Distribution.Simple.SrcDist	( sdist )
import Distribution.Simple.Register	( register, unregister,
                                          writeInstalledConfig, installedPkgConfigFile )

import Distribution.Simple.Configure(LocalBuildInfo(..), getPersistBuildConfig,
				     configure, writePersistBuildConfig, localBuildInfoFile)
import Distribution.Simple.Install(install)
import Distribution.Simple.Utils (die, removeFileRecursive, currentDir)
import Distribution.License (License(..))
import Distribution.Extension (Extension(..))
import Distribution.Version (Version(..), VersionRange(..), Dependency(..),
			     orLaterVersion, orEarlierVersion,
			     betweenVersionsInclusive)

-- Base
import System.Cmd	(rawSystem)
import System.Environment(getArgs)
import System.Exit(ExitCode(..))
import System.Directory(removeFile, doesFileExist)

import Control.Monad(when)
import Data.Maybe(isNothing)
import Data.List	( intersperse )
import System.IO (try)
import System.Console.GetOpt
import Distribution.Compat.FilePath(joinFileName)

#ifdef DEBUG
import HUnit (Test)
#endif

defaultPackageDesc :: FilePath
defaultPackageDesc = "Setup.description"

hookedPackageDesc :: FilePath
hookedPackageDesc = "Setup.buildinfo"

type Args = [String]

data UserHooks = UserHooks
    {
     runTests :: Bool -> IO ExitCode, -- ^Used for @.\/setup test@
     readDesc :: IO (Maybe PackageDescription), -- ^Read the description file

     preConf  :: Args -> ConfigFlags -> IO (Maybe PackageDescription),
     postConf :: IO ExitCode,

     preBuild  :: Args -> IO (Maybe PackageDescription),
     postBuild :: IO ExitCode,

     preClean  :: Args -> IO (Maybe PackageDescription),
     postClean :: IO ExitCode,

     preCopy  :: Args
                 -> (Maybe FilePath) -- Copy Location
                 -> IO (Maybe PackageDescription),
     postCopy :: IO ExitCode,

     preInst  :: Args -> Bool ->  IO (Maybe PackageDescription),
     postInst :: IO ExitCode, -- ^guaranteed to be run on target

     preSDist  :: Args -> IO (Maybe PackageDescription),
     postSDist :: IO ExitCode,

     preReg  :: Args
                -> Bool -- Install in the user's database?
                -> IO (Maybe PackageDescription),
     postReg :: IO ExitCode,

     preUnreg  :: Args -> IO (Maybe PackageDescription),
     postUnreg :: IO ExitCode
    }

-- |Reads local build info, executes function
doBuildInstall :: (PackageDescription -> LocalBuildInfo -> IO ()) -- ^function to apply
               -> PackageDescription
               -> IO ()
doBuildInstall f pkgConf
    = do lbi <- getPersistBuildConfig
         f pkgConf lbi

-- |Reads the package description file using IO.
defaultMain :: IO ()
defaultMain = do args <- getArgs
                 (action, args) <- parseGlobalArgs args
                 pkg_descr <- readPackageDescription defaultPackageDesc
                 defaultMainWorker pkg_descr action args Nothing
                 return ()

defaultMainWithHooks :: UserHooks
                     -> IO ()
defaultMainWithHooks hooks
    = do args <- getArgs
         (action, args) <- parseGlobalArgs args
         maybeDesc <- readDesc hooks
         case maybeDesc of
          Just pkg_descr -> defaultMainWorker pkg_descr action args (Just hooks) >> return ()
          Nothing        -> do pkg_descr <- readPackageDescription defaultPackageDesc
                               defaultMainWorker pkg_descr action args (Just hooks)
                               return ()

-- |Accept description as input rather than using IO to read it.
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
    = do case action of
            ConfigCmd flags -> do
                (flags, optFns, args) <-
			parseConfigureArgs flags args [buildDirOpt]
                pkg_descr <- hookOrInArgs preConf args flags
                no_extra_flags args
		localbuildinfo <- configure pkg_descr flags
		writePersistBuildConfig (foldr id localbuildinfo optFns)
                postHook postConf

            BuildCmd -> do
                (_, args) <- parseBuildArgs args []
                pkg_descr <- hookOrInput preBuild args
                no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		build pkg_descr localbuildinfo knownSuffixHandlers
                writeInstalledConfig pkg_descr localbuildinfo
                postHook postBuild

            CleanCmd -> do
                (_, args) <- parseCleanArgs args []
                pkg_descr <- hookOrInput preClean args
                no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		let buildPref = buildDir localbuildinfo
		try $ removeFileRecursive buildPref
                try $ removeFile installedPkgConfigFile
                try $ removeFile localBuildInfoFile
                removePreprocessedPackage pkg_descr currentDir (ppSuffixes knownSuffixHandlers)
                postHook postClean

            CopyCmd mprefix -> do
                (mprefix, _, args) <- parseCopyArgs mprefix args []
                pkg_descr <- hookOrInArgs preCopy args mprefix
                no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		install pkg_descr localbuildinfo mprefix
                postHook postCopy

            InstallCmd uInst -> do
                (uInst, _, args) <- parseInstallArgs uInst args []
                pkg_descr <- hookOrInArgs preInst args uInst
                no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
                -- FIX (HUGS): fix 'die' checks commands below.
                when (compilerFlavor (compiler (localbuildinfo)) == Hugs && uInst)
                      (die "Hugs cannot yet install user-only packages.")
		install pkg_descr localbuildinfo Nothing
                when (hasLibs pkg_descr)
                         (register pkg_descr localbuildinfo uInst)
                postHook postInst

            SDistCmd -> do
                let distPref = "dist"
                let srcPref   = distPref `joinFileName` "src"
                (_, args) <- parseSDistArgs args []
                pkg_descr <- hookOrInput preSDist args
                no_extra_flags args
		sdist srcPref distPref knownSuffixHandlers pkg_descr
                postHook postSDist

            RegisterCmd uInst -> do
                (uInst, _, args) <- parseRegisterArgs uInst args []
                pkg_descr <- hookOrInArgs preReg args uInst
                no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		when (hasLibs pkg_descr) (register pkg_descr localbuildinfo uInst)
                postHook postReg

            UnregisterCmd -> do
                (_, args) <- parseUnregisterArgs args []
                pkg_descr <- hookOrInput preUnreg args
                no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		unregister pkg_descr localbuildinfo
                postHook postUnreg
        where
        hookOrInput :: (UserHooks -> (b -> IO (Maybe PackageDescription)))
                    -> b
                    -> IO PackageDescription
        hookOrInput f i
                 = case hooks of
                    Nothing -> return pkg_descr_in
                    Just h -> do maybeDesc <- (f h) $ i
                                 case maybeDesc of
                                  Nothing -> return pkg_descr_in
                                  Just x  -> return x
        postHook f = case hooks of
                      Nothing -> return ExitSuccess
                      Just h  -> f h

        hookOrInArgs :: (UserHooks -> (a -> b -> IO (Maybe PackageDescription)))
                     -> a
                     -> b
                     -> IO PackageDescription
        hookOrInArgs f a i
                 = case hooks of
                    Nothing -> return pkg_descr_in
                    Just h -> do maybeDesc <- (f h) a i
                                 case maybeDesc of
                                  Nothing -> return pkg_descr_in
                                  Just x  -> return x

no_extra_flags :: [String] -> IO ()
no_extra_flags [] = return ()
no_extra_flags extra_flags  = 
  die ("Unrecognised flags: " ++ concat (intersperse "," (extra_flags)))

buildDirOpt :: OptDescr (LocalBuildInfo -> LocalBuildInfo)
buildDirOpt = Option "b" ["builddir"] (ReqArg setBuildDir "DIR")
		"directory to receive the built package [dist/build]"
  where setBuildDir dir lbi = lbi { buildDir = dir }

helpprefix :: String
helpprefix = "Syntax: ./Setup.hs command [flags]\n"


-- |Empty UserHooks which do nothing.
emptyUserHooks :: UserHooks
emptyUserHooks
    = UserHooks
      {
       runTests  = \_ -> res,
       readDesc  = return Nothing,
       preConf   = \_ -> rn,
       postConf  = res,
       preBuild  = rn,
       postBuild = res,
       preClean  = rn,
       postClean = res,
       preCopy   = \_ -> rn,
       postCopy  = res,
       preInst   = \_ -> rn,
       postInst  = res,
       preSDist  = rn,
       postSDist = res,
       preReg    = \_ -> rn,
       postReg   = res,
       preUnreg  = rn,
       postUnreg = res
      }
    where rn _  = return Nothing
          res = return ExitSuccess

-- |Basic default 'UserHooks' which read and write to
-- 'hookedPackageDesc'.  It's likely that unless you have a very
-- basic /configure/ script, you will want to override preConf.

defaultUserHooks :: UserHooks
defaultUserHooks
    = emptyUserHooks
      {
       preConf   = defaultPreConf,
       preBuild  = readHook,
       preClean  = readHook,
       preCopy   = \_ -> readHook,
       preInst   = \_ -> readHook,
       preSDist  = readHook,
       preReg    = \_ -> readHook,
       preUnreg  = readHook
      }
    where readHook _ = readPackageDescription hookedPackageDesc >>= (return . Just)
          defaultPreConf :: [String] -> ConfigFlags -> IO (Maybe PackageDescription)
          defaultPreConf args (_, _, _, mb_prefix)
              = do let prefix_opt pref opts = ("--prefix=" ++ pref) : opts
	           whenM (doesFileExist "configure") $
	                 rawSystem "./configure"
	                   (maybe id prefix_opt mb_prefix args)
                   outDesc <- readPackageDescription hookedPackageDesc
                   return $ Just outDesc



whenM :: IO Bool -> IO a -> IO ()
whenM cond act = do
	b <- cond
	when b $ do
		act
		return ()

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
simpleHunitTests :: [Test]
simpleHunitTests = []
#endif
