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
	defaultMain, defaultMainNoRead,
#ifdef DEBUG        
        simpleHunitTests
#endif
  ) where

-- local
import Distribution.Package --must not specify imports, since we're exporting moule.
import Distribution.Setup

import Distribution.Simple.Build	( build )
import Distribution.Simple.SrcDist	( sdist )
import Distribution.Simple.Register	( register, unregister,
                                          writeInstalledConfig, installedPkgConfigFile )

import Distribution.Simple.Configure(LocalBuildInfo(..), getPersistBuildConfig,
				     configure, writePersistBuildConfig, localBuildInfoFile)
import Distribution.Simple.Install(install)
import Distribution.Simple.Utils (die, pathJoin, removeFileRecursive)
import Distribution.Misc (License(..), Extension(..), Dependency(..))
import Distribution.Version (Version(..), VersionRange(..), 
			     orLaterVersion, orEarlierVersion,
			     betweenVersionsInclusive)

-- Base
import System.Environment(getArgs)
import System.Directory(removeFile)

import System.IO.Error(try)
import Control.Monad(when)
import Data.Maybe(isNothing)
import Data.List	( intersperse )
import System.IO (hPutStr, stderr)

#ifdef DEBUG
import HUnit (Test)
#endif

defaultPackageDesc :: FilePath
defaultPackageDesc = "Setup.description"

-- |Reads local build info, executes function
doBuildInstall :: (PackageDescription -> LocalBuildInfo -> IO ()) -- ^function to apply
               -> PackageDescription
               -> IO ()
doBuildInstall f pkgConf
    = do lbi <- getPersistBuildConfig
         f pkgConf lbi

defaultMain :: IO ()
defaultMain = parsePackageDesc defaultPackageDesc >>= defaultMainNoRead

defaultMainNoRead :: PackageDescription -> IO ()
defaultMainNoRead pkg_descr
    = do args <- getArgs
         let distPref = "dist"
         let buildPref = pathJoin [distPref, "build"]
         let srcPref = pathJoin [distPref, "src"]
         (action, args) <- parseGlobalArgs args
         case action of
            ConfigCmd flags -> do
                (flags, _, args) <- parseConfigureArgs flags args []
                no_extra_flags args
		localbuildinfo <- configure pkg_descr flags
		writePersistBuildConfig localbuildinfo

            BuildCmd -> do
                (_, args) <- parseBuildArgs args []
                no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		build buildPref pkg_descr localbuildinfo
                writeInstalledConfig pkg_descr localbuildinfo
 
            CleanCmd -> do
                (_, args) <- parseCleanArgs args []
                no_extra_flags args
		try $ removeFileRecursive buildPref
                try $ removeFile installedPkgConfigFile
                try $ removeFile localBuildInfoFile
                return ()

            InstallCmd mprefix uInst -> do
                ((mprefix,uInst), _, args) <- parseInstallArgs (mprefix,uInst) args []
                no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		install buildPref pkg_descr localbuildinfo mprefix
                when (isNothing mprefix && hasLibs pkg_descr)
                         (register pkg_descr localbuildinfo uInst)

            SDistCmd -> do
                (_, args) <- parseSDistArgs args []
                no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		sdist srcPref distPref pkg_descr localbuildinfo

            RegisterCmd uInst -> do
                (uInst, _, args) <- parseRegisterArgs uInst args []
                no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		when (hasLibs pkg_descr) (register pkg_descr localbuildinfo uInst)

            UnregisterCmd -> do
                (_, args) <- parseUnregisterArgs args []
                no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		unregister pkg_descr localbuildinfo

no_extra_flags :: [String] -> IO ()
no_extra_flags [] = return ()
no_extra_flags extra_flags  = 
  die ("Unrecognised flags: " ++ concat (intersperse "," (extra_flags)))

helpprefix :: String
helpprefix = "Syntax: ./Setup.hs command [flags]\n"

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
simpleHunitTests :: [Test]
simpleHunitTests = []
#endif
