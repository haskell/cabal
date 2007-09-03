-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Make
-- Copyright   :  Martin Sj&#xF6;gren 2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Uses the parsed command-line from Distribution.Setup in order to build
-- Haskell tools using a backend build system based on make. Obviously we
-- assume that there is a configure script, and that after the ConfigCmd has
-- been run, there is a Makefile. Further assumptions:
-- 
-- [ConfigCmd] We assume the configure script accepts
-- 		@--with-hc@,
-- 		@--with-hc-pkg@,
-- 		@--prefix@,
-- 		@--bindir@,
-- 		@--libdir@,
-- 		@--libexecdir@,
-- 		@--datadir@.
-- 
-- [BuildCmd] We assume that the default Makefile target will build everything.
-- 
-- [InstallCmd] We assume there is an @install@ target. Note that we assume that
-- this does *not* register the package!
-- 
-- [CopyCmd]	We assume there is a @copy@ target, and a variable @$(destdir)@.
-- 		The @copy@ target should probably just invoke @make install@
--		recursively (e.g. @$(MAKE) install prefix=$(destdir)\/$(prefix)
--		bindir=$(destdir)\/$(bindir)@. The reason we can\'t invoke @make
--		install@ directly here is that we don\'t know the value of @$(prefix)@.
-- 
-- [SDistCmd] We assume there is a @dist@ target.
-- 
-- [RegisterCmd] We assume there is a @register@ target and a variable @$(user)@.
-- 
-- [UnregisterCmd] We assume there is an @unregister@ target.
-- 
-- [HaddockCmd] We assume there is a @docs@ or @doc@ target.
-- 
-- [ProgramaticaCmd] We assume there is a @programatica@ target.


--			copy :
-- 				$(MAKE) install prefix=$(destdir)/$(prefix) \
-- 						bindir=$(destdir)/$(bindir) \

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
	defaultMain, defaultMainArgs, defaultMainNoRead
  ) where

-- local
import Distribution.Package --must not specify imports, since we're exporting moule.
import Distribution.Simple.Program(defaultProgramConfiguration)
import Distribution.PackageDescription
import Distribution.Simple.Setup

import Distribution.Simple.Utils (die, maybeExit)

import Distribution.License (License(..))
import Distribution.Version (Version(..))
import Distribution.Verbosity

import System.Environment(getArgs)
import Data.List  ( intersperse )
import System.Cmd
import System.Exit

exec :: String -> IO ExitCode
exec cmd = (putStrLn $ "-=-= Cabal executing: " ++ cmd ++ "=-=-")
           >> system cmd

defaultMain :: IO ()
defaultMain = getArgs >>= defaultMainArgs

defaultMainArgs :: [String] -> IO ()
defaultMainArgs args =
    defaultMainHelper args $ \ _verbosity ->
        error "Package Descripion was promised not be used here"
    --defaultPackageDesc verbosity >>=
    --    readPackageDescription verbosity 

defaultMainNoRead :: PackageDescription -> IO ()
defaultMainNoRead pkg_descr = do
    args <- getArgs
    defaultMainHelper args $ \ _ -> return pkg_descr 

defaultMainHelper :: [String] -> (Verbosity -> IO PackageDescription) -> IO ()
defaultMainHelper globalArgs _get_pkg_descr
    = do (action, args) <- parseGlobalArgs defaultProgramConfiguration globalArgs
         case action of
            ConfigCmd flags -> do
                (configFlags, _, extraArgs) <- parseConfigureArgs defaultProgramConfiguration flags args []
                retVal <- exec $ unwords $
                  "./configure" : configureArgs configFlags ++ extraArgs
                if (retVal == ExitSuccess)
                  then putStrLn "Configure Succeeded."
                  else putStrLn "Configure failed."
                exitWith retVal

            CopyCmd copydest0 -> do
                ((CopyFlags copydest _), _, extraArgs) <- parseCopyArgs (CopyFlags copydest0 normal) args []
                no_extra_flags extraArgs
		let cmd = case copydest of 
				NoCopyDest      -> "install"
				CopyTo path     -> "copy destdir=" ++ path
				CopyPrefix path -> "install prefix=" ++ path
					-- CopyPrefix is backwards compat, DEPRECATED
                maybeExit $ system $ ("make " ++ cmd)

            InstallCmd -> do
                ((InstallFlags _ _), _, extraArgs) <- parseInstallArgs emptyInstallFlags args []
                no_extra_flags extraArgs
                maybeExit $ system $ "make install"
                retVal <- exec "make register"
                if (retVal == ExitSuccess)
                  then putStrLn "Install Succeeded."
                  else putStrLn "Install failed."
                exitWith retVal

            HaddockCmd -> do 
                (_, _, extraArgs) <- parseHaddockArgs emptyHaddockFlags args []
                no_extra_flags extraArgs
                retVal <- exec "make docs"
                case retVal of
                 ExitSuccess -> do putStrLn "Haddock Succeeded"
                                   exitWith ExitSuccess
                 _ -> do retVal' <- exec "make doc"
                         case retVal' of
                          ExitSuccess -> do putStrLn "Haddock Succeeded"
                                            exitWith ExitSuccess
                          _ -> do putStrLn "Haddock Failed."
                                  exitWith retVal'

            BuildCmd -> basicCommand "Build" "make"
                                     (parseBuildArgs defaultProgramConfiguration
				        (emptyBuildFlags defaultProgramConfiguration)
					args [])

            MakefileCmd -> exitWith ExitSuccess -- presumably nothing to do

            CleanCmd -> basicCommand "Clean" "make clean"
                                     (parseCleanArgs emptyCleanFlags args [])

            SDistCmd -> basicCommand "SDist" "make dist" (parseSDistArgs args [])

            RegisterCmd  -> basicCommand "Register" "make register"
                                           (parseRegisterArgs emptyRegisterFlags args [])

            UnregisterCmd -> basicCommand "Unregister" "make unregister"
                                           (parseUnregisterArgs emptyRegisterFlags args [])
            ProgramaticaCmd -> basicCommand "Programatica" "make programatica"
                                        (parseProgramaticaArgs args [])

            HelpCmd -> exitWith ExitSuccess -- this is handled elsewhere
            
            _ -> do putStrLn "Command not implemented for makefiles."
                    exitWith (ExitFailure 1)

-- |convinience function for repetitions above
basicCommand :: String  -- ^Command name
             -> String  -- ^Command command
             -> (IO (b, [a], [String]))   -- ^Command parser function
             -> IO ()
basicCommand commandName commandCommand commandParseFun = do 
                (_, _, args) <- commandParseFun
                no_extra_flags args
                retVal <- exec commandCommand
                putStrLn $ commandName ++ 
                    if (retVal == ExitSuccess)
                       then " Succeeded."
                       else " Failed."
                exitWith retVal

no_extra_flags :: [String] -> IO ()
no_extra_flags [] = return ()
no_extra_flags extra_flags  = 
  die $ "Unrecognised flags: " ++ concat (intersperse "," (extra_flags))
