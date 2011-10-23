-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Make
-- Copyright   :  Martin Sj&#xF6;gren 2004
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is an alternative build system that delegates everything to the @make@
-- program. All the commands just end up calling @make@ with appropriate
-- arguments. The intention was to allow preexisting packages that used
-- makefiles to be wrapped into Cabal packages. In practice essentially all
-- such packages were converted over to the \"Simple\" build system instead.
-- Consequently this module is not used much and it certainly only sees cursory
-- maintenance and no testing. Perhaps at some point we should stop pretending
-- that it works.
--
-- Uses the parsed command-line from "Distribution.Simple.Setup" in order to build
-- Haskell tools using a backend build system based on make. Obviously we
-- assume that there is a configure script, and that after the ConfigCmd has
-- been run, there is a Makefile. Further assumptions:
--
-- [ConfigCmd] We assume the configure script accepts
--              @--with-hc@,
--              @--with-hc-pkg@,
--              @--prefix@,
--              @--bindir@,
--              @--libdir@,
--              @--libexecdir@,
--              @--datadir@.
--
-- [BuildCmd] We assume that the default Makefile target will build everything.
--
-- [InstallCmd] We assume there is an @install@ target. Note that we assume that
-- this does *not* register the package!
--
-- [CopyCmd]    We assume there is a @copy@ target, and a variable @$(destdir)@.
--              The @copy@ target should probably just invoke @make install@
--              recursively (e.g. @$(MAKE) install prefix=$(destdir)\/$(prefix)
--              bindir=$(destdir)\/$(bindir)@. The reason we can\'t invoke @make
--              install@ directly here is that we don\'t know the value of @$(prefix)@.
--
-- [SDistCmd] We assume there is a @dist@ target.
--
-- [RegisterCmd] We assume there is a @register@ target and a variable @$(user)@.
--
-- [UnregisterCmd] We assume there is an @unregister@ target.
--
-- [HaddockCmd] We assume there is a @docs@ or @doc@ target.


--                      copy :
--                              $(MAKE) install prefix=$(destdir)/$(prefix) \
--                                              bindir=$(destdir)/$(bindir) \

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
import Distribution.Compat.Exception
import Distribution.Package --must not specify imports, since we're exporting moule.
import Distribution.Simple.Program(defaultProgramConfiguration)
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.Command

import Distribution.Simple.Utils (rawSystemExit, cabalVersion)

import Distribution.License (License(..))
import Distribution.Version
         ( Version(..) )
import Distribution.Text
         ( display )

import System.Environment (getArgs, getProgName)
import Data.List  (intersperse)
import System.Exit

defaultMain :: IO ()
defaultMain = getArgs >>= defaultMainArgs

defaultMainArgs :: [String] -> IO ()
defaultMainArgs = defaultMainHelper

{-# DEPRECATED defaultMainNoRead "it ignores its PackageDescription arg" #-}
defaultMainNoRead :: PackageDescription -> IO ()
defaultMainNoRead = const defaultMain

defaultMainHelper :: [String] -> IO ()
defaultMainHelper args =
  case commandsRun globalCommand commands args of
    CommandHelp   help                 -> printHelp help
    CommandList   opts                 -> printOptionsList opts
    CommandErrors errs                 -> printErrors errs
    CommandReadyToGo (flags, commandParse)  ->
      case commandParse of
        _ | fromFlag (globalVersion flags)        -> printVersion
          | fromFlag (globalNumericVersion flags) -> printNumericVersion
        CommandHelp     help           -> printHelp help
        CommandList     opts           -> printOptionsList opts
        CommandErrors   errs           -> printErrors errs
        CommandReadyToGo action        -> action

  where
    printHelp help = getProgName >>= putStr . help
    printOptionsList = putStr . unlines
    printErrors errs = do
      putStr (concat (intersperse "\n" errs))
      exitWith (ExitFailure 1)
    printNumericVersion = putStrLn $ display cabalVersion
    printVersion        = putStrLn $ "Cabal library version "
                                  ++ display cabalVersion

    progs = defaultProgramConfiguration
    commands =
      [configureCommand progs `commandAddAction` configureAction
      ,buildCommand     progs `commandAddAction` buildAction
      ,installCommand         `commandAddAction` installAction
      ,copyCommand            `commandAddAction` copyAction
      ,haddockCommand         `commandAddAction` haddockAction
      ,cleanCommand           `commandAddAction` cleanAction
      ,sdistCommand           `commandAddAction` sdistAction
      ,registerCommand        `commandAddAction` registerAction
      ,unregisterCommand      `commandAddAction` unregisterAction
      ]

configureAction :: ConfigFlags -> [String] -> IO ()
configureAction flags args = do
  noExtraFlags args
  let verbosity = fromFlag (configVerbosity flags)
  rawSystemExit verbosity "sh" $
    "configure"
    : configureArgs backwardsCompatHack flags
  where backwardsCompatHack = True

copyAction :: CopyFlags -> [String] -> IO ()
copyAction flags args = do
  noExtraFlags args
  let destArgs = case fromFlag $ copyDest flags of
        NoCopyDest      -> ["install"]
        CopyTo path     -> ["copy", "destdir=" ++ path]
  rawSystemExit (fromFlag $ copyVerbosity flags) "make" destArgs

installAction :: InstallFlags -> [String] -> IO ()
installAction flags args = do
  noExtraFlags args
  rawSystemExit (fromFlag $ installVerbosity flags) "make" ["install"]
  rawSystemExit (fromFlag $ installVerbosity flags) "make" ["register"]

haddockAction :: HaddockFlags -> [String] -> IO ()
haddockAction flags args = do
  noExtraFlags args
  rawSystemExit (fromFlag $ haddockVerbosity flags) "make" ["docs"]
    `catchIO` \_ ->
    rawSystemExit (fromFlag $ haddockVerbosity flags) "make" ["doc"]

buildAction :: BuildFlags -> [String] -> IO ()
buildAction flags args = do
  noExtraFlags args
  rawSystemExit (fromFlag $ buildVerbosity flags) "make" []

cleanAction :: CleanFlags -> [String] -> IO ()
cleanAction flags args = do
  noExtraFlags args
  rawSystemExit (fromFlag $ cleanVerbosity flags) "make" ["clean"]

sdistAction :: SDistFlags -> [String] -> IO ()
sdistAction flags args = do
  noExtraFlags args
  rawSystemExit (fromFlag $ sDistVerbosity flags) "make" ["dist"]

registerAction :: RegisterFlags -> [String] -> IO ()
registerAction  flags args = do
  noExtraFlags args
  rawSystemExit (fromFlag $ regVerbosity flags) "make" ["register"]

unregisterAction :: RegisterFlags -> [String] -> IO ()
unregisterAction flags args = do
  noExtraFlags args
  rawSystemExit (fromFlag $ regVerbosity flags) "make" ["unregister"]
