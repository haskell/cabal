{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Setup.Build
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the build command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.Build
  ( BuildFlags (..)
  , emptyBuildFlags
  , defaultBuildFlags
  , buildCommand
  , DumpBuildInfo (..)
  , buildOptions
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Flag
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Types.DumpBuildInfo
import Distribution.Verbosity

import Distribution.Simple.Setup.Common

-- ------------------------------------------------------------

-- * Build flags

-- ------------------------------------------------------------

data BuildFlags = BuildFlags
  { buildProgramPaths :: [(String, FilePath)]
  , buildProgramArgs :: [(String, [String])]
  , buildDistPref :: Flag FilePath
  , buildVerbosity :: Flag Verbosity
  , buildNumJobs :: Flag (Maybe Int)
  , -- TODO: this one should not be here, it's just that the silly
    -- UserHooks stop us from passing extra info in other ways
    buildArgs :: [String]
  , buildCabalFilePath :: Flag FilePath
  }
  deriving (Read, Show, Generic, Typeable)

defaultBuildFlags :: BuildFlags
defaultBuildFlags =
  BuildFlags
    { buildProgramPaths = mempty
    , buildProgramArgs = []
    , buildDistPref = mempty
    , buildVerbosity = Flag normal
    , buildNumJobs = mempty
    , buildArgs = []
    , buildCabalFilePath = mempty
    }

buildCommand :: ProgramDb -> CommandUI BuildFlags
buildCommand progDb =
  CommandUI
    { commandName = "build"
    , commandSynopsis = "Compile all/specific components."
    , commandDescription = Just $ \_ ->
        wrapText $
          "Components encompass executables, tests, and benchmarks.\n"
            ++ "\n"
            ++ "Affected by configuration options, see `configure`.\n"
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " build           "
          ++ "    All the components in the package\n"
          ++ "  "
          ++ pname
          ++ " build foo       "
          ++ "    A component (i.e. lib, exe, test suite)\n\n"
          ++ programFlagsDescription progDb
    , -- TODO: re-enable once we have support for module/file targets
      --        ++ "  " ++ pname ++ " build Foo.Bar   "
      --        ++ "    A module\n"
      --        ++ "  " ++ pname ++ " build Foo/Bar.hs"
      --        ++ "    A file\n\n"
      --        ++ "If a target is ambiguous it can be qualified with the component "
      --        ++ "name, e.g.\n"
      --        ++ "  " ++ pname ++ " build foo:Foo.Bar\n"
      --        ++ "  " ++ pname ++ " build testsuite1:Foo/Bar.hs\n"
      commandUsage =
        usageAlternatives "build" $
          [ "[FLAGS]"
          , "COMPONENTS [FLAGS]"
          ]
    , commandDefaultFlags = defaultBuildFlags
    , commandOptions = \showOrParseArgs ->
        [ optionVerbosity
            buildVerbosity
            (\v flags -> flags{buildVerbosity = v})
        , optionDistPref
            buildDistPref
            (\d flags -> flags{buildDistPref = d})
            showOrParseArgs
        ]
          ++ buildOptions progDb showOrParseArgs
    }

buildOptions
  :: ProgramDb
  -> ShowOrParseArgs
  -> [OptionField BuildFlags]
buildOptions progDb showOrParseArgs =
  [ optionNumJobs
      buildNumJobs
      (\v flags -> flags{buildNumJobs = v})
  ]
    ++ programDbPaths
      progDb
      showOrParseArgs
      buildProgramPaths
      (\v flags -> flags{buildProgramPaths = v})
    ++ programDbOption
      progDb
      showOrParseArgs
      buildProgramArgs
      (\v fs -> fs{buildProgramArgs = v})
    ++ programDbOptions
      progDb
      showOrParseArgs
      buildProgramArgs
      (\v flags -> flags{buildProgramArgs = v})

emptyBuildFlags :: BuildFlags
emptyBuildFlags = mempty

instance Monoid BuildFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup BuildFlags where
  (<>) = gmappend
