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
-- Explanation: <FIX>
-- WHERE DOES THIS MODULE FIT IN AT A HIGH-LEVEL <FIX>

{- Copyright (c) 2003-2004, Isaac Jones
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
import Distribution.Setup(parseArgs, Action(..), optionHelpString)

import Distribution.Simple.Build	( build )
import Distribution.Simple.SrcDist	( sdist )
import Distribution.Simple.Register	( register, unregister, installedPkgConfigFile )

import Distribution.Simple.Configure(LocalBuildInfo(..), getPersistBuildConfig,
				     configure, writePersistBuildConfig, localBuildInfoFile)
import Distribution.Simple.Install(install)
import Distribution.Simple.Utils (die, pathJoin, removeFileRecursive)
import Distribution.Misc (License(..), Extension(..), Dependency(..))
import Distribution.Version (Version(..), VersionRange(..), 
			     orLaterVersion, orEarlierVersion,
			     betweenVersionsInclusive)

-- Base
import System(getArgs)
import System.Directory(removeFile)

import Control.Exception(try)
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
         case parseArgs args of
	     Right (HelpCmd, _) -> hPutStr stderr (optionHelpString helpprefix)

	     Right (ConfigCmd flags, extra_flags) -> do
		no_extra_flags extra_flags
		localbuildinfo <- configure pkg_descr flags
		writePersistBuildConfig localbuildinfo

             Right (BuildCmd, extra_flags) -> do
		no_extra_flags extra_flags
		localbuildinfo <- getPersistBuildConfig
		build buildPref pkg_descr localbuildinfo

             Right (CleanCmd, extra_flags) -> do
		no_extra_flags extra_flags
		try $ removeFileRecursive buildPref
                try $ removeFile installedPkgConfigFile
                try $ removeFile localBuildInfoFile
                return ()

             Right (InstallCmd install_prefixM userInst, extra_flags) -> do
		no_extra_flags extra_flags
		localbuildinfo <- getPersistBuildConfig
		install buildPref pkg_descr localbuildinfo install_prefixM
                when (isNothing install_prefixM && hasLibs pkg_descr)
                         (register pkg_descr localbuildinfo userInst)

             Right (SDistCmd, extra_flags) -> do
		no_extra_flags extra_flags
		localbuildinfo <- getPersistBuildConfig
		sdist srcPref distPref pkg_descr localbuildinfo

             Right (RegisterCmd userFlag, extra_flags) -> do
		no_extra_flags extra_flags
		localbuildinfo <- getPersistBuildConfig
		when (hasLibs pkg_descr) (register pkg_descr localbuildinfo userFlag)

             Right (UnregisterCmd, extra_flags) -> do
		no_extra_flags extra_flags
		localbuildinfo <- getPersistBuildConfig
		unregister pkg_descr localbuildinfo

             Left err -> do 
		hPutStr stderr (unlines err)
		hPutStr stderr (optionHelpString helpprefix)
         return ()

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
