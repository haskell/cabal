-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Make
-- Copyright   :  Martin Sjögren 2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Explanation: <FIX>
-- WHERE DOES THIS MODULE FIT IN AT A HIGH-LEVEL <FIX>

{- Copyright (c) 2004, Martin Sjögren
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

module Distribution.Make (
	module Distribution.Package,
	License(..), Version(..),
	defaultMain,
  ) where

-- local
import Distribution.Package --must not specify imports, since we're exporting moule.
import Distribution.Setup --(parseArgs, Action(..), optionHelpString)

import Distribution.Simple.Utils (maybeExit)

import Distribution.Misc (License(..))
import Distribution.Version (Version(..))

-- Base
import System(getArgs)

import Control.Monad (when, unless)
import Data.Maybe (isNothing, maybe)
import Data.List	( intersperse )
import System.IO (hPutStr, stderr)
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
configureArgs (_, Just hc_path, maybe_prefix)
    = "--with-hc=" ++ hc_path ++ maybe "" (" --prefix="++) maybe_prefix
configureArgs (Just hc, Nothing, maybe_prefix)
    = "--with-hc=" ++ showHC hc ++ maybe "" (" --prefix="++) maybe_prefix
  where showHC GHC = "ghc"
        showHC NHC = "nhc98"
        showHC Hugs = "hugs"
configureArgs (Nothing, Nothing, maybe_prefix)
    = maybe "" ("--prefix="++) maybe_prefix

exec :: String -> IO a
exec cmd = system cmd >>= exitWith

defaultMain :: PackageDescription -> IO ()
defaultMain pkg_descr
    = do args <- getArgs
         case parseArgs args of
	     Right (HelpCmd, _) -> hPutStr stderr (optionHelpString helpprefix)

	     Right (ConfigCmd flags, extra_flags) -> do
		no_extra_flags extra_flags
                exec $ "./configure " ++ configureArgs flags

             Right (BuildCmd, extra_flags) -> do
		no_extra_flags extra_flags
                exec "make"

             Right (InstallCmd install_prefixM userInst, extra_flags) -> do
		no_extra_flags extra_flags
                maybeExit $ system $ "make install" ++
                                     maybe "" (" prefix="++) install_prefixM
                when (isNothing install_prefixM) (exec "make register")

             Right (SDistCmd, extra_flags) -> do
		no_extra_flags extra_flags
                exec "make dist"

             Right (RegisterCmd userFlag, extra_flags) -> do
		no_extra_flags extra_flags
                exec "make register"

             Right (UnregisterCmd, extra_flags) -> do
		no_extra_flags extra_flags
                exec "make unregister"

             Left err -> do 
		hPutStr stderr (unlines err)
		hPutStr stderr (optionHelpString helpprefix)
         return ()

no_extra_flags :: [String] -> IO ()
no_extra_flags [] = return ()
no_extra_flags extra_flags  = 
  do hPutStr stderr $ "Unrecognised flags: " ++ concat (intersperse "," (extra_flags))
     exitWith (ExitFailure 1)

helpprefix :: String
helpprefix = "Syntax: ./Setup.hs command [flags]\n"
