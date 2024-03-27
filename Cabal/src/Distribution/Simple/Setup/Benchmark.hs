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
-- Module      :  Distribution.Simple.Benchmark
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the benchmarking command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.Benchmark
  ( BenchmarkFlags
      ( BenchmarkCommonFlags
      , benchmarkVerbosity
      , benchmarkDistPref
      , benchmarkCabalFilePath
      , benchmarkWorkingDir
      , benchmarkTargets
      , ..
      )
  , emptyBenchmarkFlags
  , defaultBenchmarkFlags
  , benchmarkCommand
  , benchmarkOptions'
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.InstallDirs
import Distribution.Simple.Setup.Common
import Distribution.Simple.Utils
import Distribution.Utils.Path
import Distribution.Verbosity

-- ------------------------------------------------------------

-- * Benchmark flags

-- ------------------------------------------------------------

data BenchmarkFlags = BenchmarkFlags
  { benchmarkCommonFlags :: !CommonSetupFlags
  , benchmarkOptions :: [PathTemplate]
  }
  deriving (Show, Generic, Typeable)

pattern BenchmarkCommonFlags
  :: Flag Verbosity
  -> Flag (SymbolicPath Pkg (Dir Dist))
  -> Flag (SymbolicPath CWD (Dir Pkg))
  -> Flag (SymbolicPath Pkg File)
  -> [String]
  -> BenchmarkFlags
pattern BenchmarkCommonFlags
  { benchmarkVerbosity
  , benchmarkDistPref
  , benchmarkWorkingDir
  , benchmarkCabalFilePath
  , benchmarkTargets
  } <-
  ( benchmarkCommonFlags ->
      CommonSetupFlags
        { setupVerbosity = benchmarkVerbosity
        , setupDistPref = benchmarkDistPref
        , setupWorkingDir = benchmarkWorkingDir
        , setupCabalFilePath = benchmarkCabalFilePath
        , setupTargets = benchmarkTargets
        }
    )

instance Binary BenchmarkFlags
instance Structured BenchmarkFlags

defaultBenchmarkFlags :: BenchmarkFlags
defaultBenchmarkFlags =
  BenchmarkFlags
    { benchmarkCommonFlags = defaultCommonSetupFlags
    , benchmarkOptions = []
    }

benchmarkCommand :: CommandUI BenchmarkFlags
benchmarkCommand =
  CommandUI
    { commandName = "bench"
    , commandSynopsis =
        "Run all/specific benchmarks."
    , commandDescription = Just $ \_pname ->
        wrapText $
          testOrBenchmarkHelpText "benchmark"
    , commandNotes = Nothing
    , commandUsage =
        usageAlternatives
          "bench"
          [ "[FLAGS]"
          , "BENCHCOMPONENTS [FLAGS]"
          ]
    , commandDefaultFlags = defaultBenchmarkFlags
    , commandOptions = benchmarkOptions'
    }

benchmarkOptions' :: ShowOrParseArgs -> [OptionField BenchmarkFlags]
benchmarkOptions' showOrParseArgs =
  withCommonSetupOptions
    benchmarkCommonFlags
    (\c f -> f{benchmarkCommonFlags = c})
    showOrParseArgs
    [ option
        []
        ["benchmark-options"]
        ( "give extra options to benchmark executables "
            ++ "(name templates can use $pkgid, $compiler, "
            ++ "$os, $arch, $benchmark)"
        )
        benchmarkOptions
        (\v flags -> flags{benchmarkOptions = v})
        ( reqArg'
            "TEMPLATES"
            (map toPathTemplate . splitArgs)
            (const [])
        )
    , option
        []
        ["benchmark-option"]
        ( "give extra option to benchmark executables "
            ++ "(no need to quote options containing spaces, "
            ++ "name template can use $pkgid, $compiler, "
            ++ "$os, $arch, $benchmark)"
        )
        benchmarkOptions
        (\v flags -> flags{benchmarkOptions = v})
        ( reqArg'
            "TEMPLATE"
            (\x -> [toPathTemplate x])
            (map fromPathTemplate)
        )
    ]

emptyBenchmarkFlags :: BenchmarkFlags
emptyBenchmarkFlags = mempty

instance Monoid BenchmarkFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup BenchmarkFlags where
  (<>) = gmappend
