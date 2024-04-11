{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

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
  ( BuildFlags
      ( BuildCommonFlags
      , buildVerbosity
      , buildDistPref
      , buildCabalFilePath
      , buildWorkingDir
      , buildTargets
      , ..
      )
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
import Distribution.Simple.Setup.Common
import Distribution.Simple.Utils
import Distribution.Types.DumpBuildInfo
import Distribution.Utils.Path
import Distribution.Verbosity

-- ------------------------------------------------------------

-- * Build flags

-- ------------------------------------------------------------

data BuildFlags = BuildFlags
  { buildCommonFlags :: !CommonSetupFlags
  , buildProgramPaths :: [(String, FilePath)]
  , buildProgramArgs :: [(String, [String])]
  , buildNumJobs :: Flag (Maybe Int)
  , buildUseSemaphore :: Flag String
  }
  deriving (Read, Show, Generic, Typeable)

pattern BuildCommonFlags
  :: Flag Verbosity
  -> Flag (SymbolicPath Pkg (Dir Dist))
  -> Flag (SymbolicPath CWD (Dir Pkg))
  -> Flag (SymbolicPath Pkg File)
  -> [String]
  -> BuildFlags
pattern BuildCommonFlags
  { buildVerbosity
  , buildDistPref
  , buildWorkingDir
  , buildCabalFilePath
  , buildTargets
  } <-
  ( buildCommonFlags ->
      CommonSetupFlags
        { setupVerbosity = buildVerbosity
        , setupDistPref = buildDistPref
        , setupWorkingDir = buildWorkingDir
        , setupCabalFilePath = buildCabalFilePath
        , setupTargets = buildTargets
        }
    )

instance Binary BuildFlags
instance Structured BuildFlags

defaultBuildFlags :: BuildFlags
defaultBuildFlags =
  BuildFlags
    { buildCommonFlags = defaultCommonSetupFlags
    , buildProgramPaths = mempty
    , buildProgramArgs = []
    , buildNumJobs = mempty
    , buildUseSemaphore = NoFlag
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
    , commandOptions = buildOptions progDb
    }

buildOptions
  :: ProgramDb
  -> ShowOrParseArgs
  -> [OptionField BuildFlags]
buildOptions progDb showOrParseArgs =
  withCommonSetupOptions
    buildCommonFlags
    (\c f -> f{buildCommonFlags = c})
    showOrParseArgs
    ( [ optionNumJobs
          buildNumJobs
          (\v flags -> flags{buildNumJobs = v})
      , option
          []
          ["semaphore"]
          "semaphore"
          buildUseSemaphore
          (\v flags -> flags{buildUseSemaphore = v})
          (reqArg' "SEMAPHORE" Flag flagToList)
      ]
    )
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
