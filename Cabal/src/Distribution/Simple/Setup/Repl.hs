{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Setup.Repl
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the repl command-line options.
-- See: @Distribution.Simple.Setup@

module Distribution.Simple.Setup.Repl (

  ReplFlags(..),                         defaultReplFlags,     replCommand,
  ReplOptions(..),
  replOptions,
  ) where

import Prelude ()
import Distribution.Compat.Prelude hiding (get)

import Distribution.ReadE
import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Flag
import Distribution.Simple.Utils
import Distribution.Simple.Program
import Distribution.Verbosity

import Distribution.Simple.Setup.Common

-- ------------------------------------------------------------
-- * REPL Flags
-- ------------------------------------------------------------

data ReplOptions = ReplOptions {
    replOptionsFlags :: [String],
    replOptionsNoLoad :: Flag Bool,
    replOptionsFlagOutput :: Flag FilePath
  }
  deriving (Show, Generic, Typeable)

instance Binary ReplOptions
instance Structured ReplOptions


instance Monoid ReplOptions where
  mempty = ReplOptions mempty (Flag False) NoFlag
  mappend = (<>)

instance Semigroup ReplOptions where
  (<>) = gmappend

data ReplFlags = ReplFlags {
    replProgramPaths :: [(String, FilePath)],
    replProgramArgs :: [(String, [String])],
    replDistPref    :: Flag FilePath,
    replVerbosity   :: Flag Verbosity,
    replReload      :: Flag Bool,
    replReplOptions :: ReplOptions
  }
  deriving (Show, Generic, Typeable)

defaultReplFlags :: ReplFlags
defaultReplFlags  = ReplFlags {
    replProgramPaths = mempty,
    replProgramArgs = [],
    replDistPref    = NoFlag,
    replVerbosity   = Flag normal,
    replReload      = Flag False,
    replReplOptions = mempty
  }

instance Monoid ReplFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ReplFlags where
  (<>) = gmappend

replCommand :: ProgramDb -> CommandUI ReplFlags
replCommand progDb = CommandUI
  { commandName         = "repl"
  , commandSynopsis     =
      "Open an interpreter session for the given component."
  , commandDescription  = Just $ \pname -> wrapText $
         "If the current directory contains no package, ignores COMPONENT "
      ++ "parameters and opens an interactive interpreter session; if a "
      ++ "sandbox is present, its package database will be used.\n"
      ++ "\n"
      ++ "Otherwise, (re)configures with the given or default flags, and "
      ++ "loads the interpreter with the relevant modules. For executables, "
      ++ "tests and benchmarks, loads the main module (and its "
      ++ "dependencies); for libraries all exposed/other modules.\n"
      ++ "\n"
      ++ "The default component is the library itself, or the executable "
      ++ "if that is the only component.\n"
      ++ "\n"
      ++ "Support for loading specific modules is planned but not "
      ++ "implemented yet. For certain scenarios, `" ++ pname
      ++ " exec -- ghci :l Foo` may be used instead. Note that `exec` will "
      ++ "not (re)configure and you will have to specify the location of "
      ++ "other modules, if required.\n"

  , commandNotes        = Just $ \pname ->
         "Examples:\n"
      ++ "  " ++ pname ++ " repl           "
      ++ "    The first component in the package\n"
      ++ "  " ++ pname ++ " repl foo       "
      ++ "    A named component (i.e. lib, exe, test suite)\n"
      ++ "  " ++ pname ++ " repl --repl-options=\"-lstdc++\""
      ++ "  Specifying flags for interpreter\n"
--TODO: re-enable once we have support for module/file targets
--        ++ "  " ++ pname ++ " repl Foo.Bar   "
--        ++ "    A module\n"
--        ++ "  " ++ pname ++ " repl Foo/Bar.hs"
--        ++ "    A file\n\n"
--        ++ "If a target is ambiguous it can be qualified with the component "
--        ++ "name, e.g.\n"
--        ++ "  " ++ pname ++ " repl foo:Foo.Bar\n"
--        ++ "  " ++ pname ++ " repl testsuite1:Foo/Bar.hs\n"
  , commandUsage =  \pname -> "Usage: " ++ pname ++ " repl [COMPONENT] [FLAGS]\n"
  , commandDefaultFlags = defaultReplFlags
  , commandOptions = \showOrParseArgs ->
      optionVerbosity replVerbosity (\v flags -> flags { replVerbosity = v })
      : optionDistPref
          replDistPref (\d flags -> flags { replDistPref = d })
          showOrParseArgs

      : programDbPaths   progDb showOrParseArgs
          replProgramPaths (\v flags -> flags { replProgramPaths = v})

     ++ programDbOption progDb showOrParseArgs
          replProgramArgs (\v flags -> flags { replProgramArgs = v})

     ++ programDbOptions progDb showOrParseArgs
          replProgramArgs (\v flags -> flags { replProgramArgs = v})

     ++ case showOrParseArgs of
          ParseArgs ->
            [ option "" ["reload"]
              "Used from within an interpreter to update files."
              replReload (\v flags -> flags { replReload = v })
              trueArg
            ]
          _ -> []
     ++ map liftReplOption (replOptions showOrParseArgs)
  }
  where
    liftReplOption = liftOption replReplOptions (\v flags -> flags { replReplOptions = v })

replOptions :: ShowOrParseArgs -> [OptionField ReplOptions]
replOptions _ =
  [ option [] ["repl-no-load"]
    "Disable loading of project modules at REPL startup."
    replOptionsNoLoad (\p flags -> flags { replOptionsNoLoad = p })
    trueArg
  , option [] ["repl-options"]
    "Use the option(s) for the repl"
    replOptionsFlags (\p flags -> flags { replOptionsFlags = p })
    (reqArg "FLAG" (succeedReadE words) id)
  , option [] ["repl-multi-file"]
    "Write repl options to this directory rather than starting repl mode"
    replOptionsFlagOutput (\p flags -> flags { replOptionsFlagOutput = p })
    (reqArg "DIR" (succeedReadE Flag) flagToList)
  ]

