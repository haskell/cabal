-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Make
-- Copyright   :  Martin Sj&#xF6;gren 2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Explanation: Uses the parsed command-line from Distribution.Setup
-- in order to build haskell tools using a backend build system based
-- on Make.

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

module Distribution.Make (
	module Distribution.Package,
	License(..), Version(..),
	defaultMain, defaultMainNoRead
  ) where

-- local
import Distribution.Package --must not specify imports, since we're exporting moule.
import Distribution.PackageDescription
import Distribution.Setup --(parseArgs, Action(..), optionHelpString)

import Distribution.Simple.Utils (maybeExit, defaultPackageDesc)

import Distribution.License (License(..))
import Distribution.Version (Version(..))

import System.Environment(getArgs)
import Distribution.GetOpt(OptDescr)
import Control.Monad (unless)
import Data.List  ( intersperse )
import System.IO (hPutStrLn, stderr)
import System.Cmd
import System.Exit

{-
Basic assumptions
-----------------
Obviously we assume that there is a configure script, and that after the
ConfigCmd has been run, there is a Makefile.

ConfigCmd:     We assume the configure script accepts a --with-hc flag
BuildCmd:      We assume the default Makefile target will build everything
InstallCmd:    We assume there is an install target and a variable $(prefix)
               that can be overridden
               (./Setup --install-prefix=foo  ->   make prefix=foo install)
               Note that we assume that this does *not* register the package!
SDistCmd:      We assume there is an dist target
RegisterCmd:   We assume there is a register target and a variable $(user)
UnregisterCmd: We assume there is an unregister target
-}

configureArgs :: ConfigFlags -> String
configureArgs flags
  = unwords (hc_flag ++ hc_pkg_flag ++ prefix_flag)
  where
	hc_flag = case (configHcFlavor flags, configHcPath flags) of
			(_, Just hc_path)  -> ["--with-hc=" ++ hc_path]
			(Just hc, Nothing) -> ["--with-hc=" ++ showHC hc]
			(Nothing,Nothing)  -> []
	hc_pkg_flag = case configHcPkg flags of
			Just hc_pkg_path -> ["--with-hc-pkg=" ++ hc_pkg_path]
			Nothing          -> []
	prefix_flag = case configPrefix flags of
			Just p  -> ["--prefix=" ++ p]
			Nothing -> []

  	showHC GHC = "ghc"
        showHC NHC = "nhc98"
        showHC Hugs = "hugs"

exec :: String -> IO ExitCode
exec cmd = (putStrLn $ "-=-= Cabal executing: " ++ cmd ++ "=-=-")
           >> system cmd

defaultMain :: IO ()
defaultMain = defaultPackageDesc >>= readPackageDescription >>= defaultMainNoRead

defaultMainNoRead :: PackageDescription -> IO ()
defaultMainNoRead pkg_descr
    = do args <- getArgs
         (action, args) <- parseGlobalArgs args
         case action of
            ConfigCmd flags -> do
                (flags, _, args) <- parseConfigureArgs flags args []
                no_extra_flags args
                retVal <- exec $ "./configure " ++ configureArgs flags
                if (retVal == ExitSuccess)
                  then putStrLn "Configure Succeeded."
                  else putStrLn "Configure failed."
                exitWith retVal

            BuildCmd -> do
                (_, _, args) <- parseBuildArgs args []
                no_extra_flags args
                retVal <- exec "make"
                if (retVal == ExitSuccess)
                  then putStrLn "Build Succeeded."
                  else putStrLn "Build failed."
                exitWith retVal

            CleanCmd -> do
                (_, _, args) <- parseCleanArgs args []
                no_extra_flags args
                retVal <- exec "make clean"
                if (retVal == ExitSuccess)
                  then putStrLn "Clean Succeeded."
                  else putStrLn "Clean failed."
                exitWith retVal

            CopyCmd mprefix -> do
                ((mprefix,verbose), _, args) <- parseCopyArgs (mprefix,0) args []
                no_extra_flags args
                maybeExit $ system $ "make install" ++
                                     maybe "" (" prefix="++) mprefix

            InstallCmd uInst -> do
                ((uInst,verbose), _, args) <- parseInstallArgs (uInst,0) args []
                no_extra_flags args
                maybeExit $ system $ "make install"
                retVal <- exec "make register"
                if (retVal == ExitSuccess)
                  then putStrLn "Install Succeeded."
                  else putStrLn "Install failed."
                exitWith retVal

            SDistCmd -> do
                (_, _, args) <- parseSDistArgs args []
                no_extra_flags args
                retVal <- exec "make dist"
                if (retVal == ExitSuccess)
                  then putStrLn "Sdist Succeeded."
                  else putStrLn "Sdist failed."
                exitWith retVal

            RegisterCmd uInst -> do
                ((uInst,0), _, args) <- parseRegisterArgs (uInst,0) args []
                no_extra_flags args
                retVal <- exec "make register"
                if (retVal == ExitSuccess)
                  then putStrLn "Register Succeeded."
                  else putStrLn "Register failed."
                exitWith retVal

            UnregisterCmd -> do
                (_, _, args) <- parseUnregisterArgs args []
                no_extra_flags args
                retVal <- exec "make unregister"
                if (retVal == ExitSuccess)
                  then putStrLn "Unregister Succeeded."
                  else putStrLn "Unregister failed."
                exitWith retVal
            cmd -> do 
                error $ "Simple Cabal Makefile interface doesn't support command: " ++ (show cmd)

no_extra_flags :: [String] -> IO ()
no_extra_flags [] = return ()
no_extra_flags extra_flags  = 
  do hPutStrLn stderr $ "Unrecognised flags: " ++ concat (intersperse "," (extra_flags))
     exitWith (ExitFailure 1)
