{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

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
  ( BenchmarkFlags (..)
  , emptyBenchmarkFlags
  , defaultBenchmarkFlags
  , benchmarkCommand
  , benchmarkOptions'
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs
import Distribution.Simple.Utils
import Distribution.Verbosity

import Distribution.Simple.Setup.Common

-- ------------------------------------------------------------

-- * Benchmark flags

-- ------------------------------------------------------------

data BenchmarkFlags = BenchmarkFlags
  { benchmarkDistPref :: Flag FilePath
  , benchmarkVerbosity :: Flag Verbosity
  , benchmarkOptions :: [PathTemplate]
  }
  deriving (Show, Generic, Typeable)

defaultBenchmarkFlags :: BenchmarkFlags
defaultBenchmarkFlags =
  BenchmarkFlags
    { benchmarkDistPref = NoFlag
    , benchmarkVerbosity = Flag normal
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
  [ optionVerbosity
      benchmarkVerbosity
      (\v flags -> flags{benchmarkVerbosity = v})
  , optionDistPref
      benchmarkDistPref
      (\d flags -> flags{benchmarkDistPref = d})
      showOrParseArgs
  , option
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
