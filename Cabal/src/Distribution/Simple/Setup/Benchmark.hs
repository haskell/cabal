{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

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
  , benchmarkTargets :: [String]
  -- ^ The targets of this invocation, given as positional arguments.
  , benchmarkOptions :: [PathTemplate]
  }
  deriving (Show, Generic)
  deriving (Semigroup, Monoid) via Generically BenchmarkFlags

pattern BenchmarkCommonFlags
  :: Flag VerbosityFlags
  -> Flag (SymbolicPath Pkg (Dir Dist))
  -> Flag (SymbolicPath CWD (Dir Pkg))
  -> Flag (SymbolicPath Pkg File)
  -> BenchmarkFlags
pattern BenchmarkCommonFlags
  { benchmarkVerbosity
  , benchmarkDistPref
  , benchmarkWorkingDir
  , benchmarkCabalFilePath
  } <-
  ( benchmarkCommonFlags ->
      CommonSetupFlags
        { setupVerbosity = benchmarkVerbosity
        , setupDistPref = benchmarkDistPref
        , setupWorkingDir = benchmarkWorkingDir
        , setupCabalFilePath = benchmarkCabalFilePath
        }
    )

instance Binary BenchmarkFlags
instance Structured BenchmarkFlags

defaultBenchmarkFlags :: BenchmarkFlags
defaultBenchmarkFlags =
  BenchmarkFlags
    { benchmarkCommonFlags = defaultCommonSetupFlags
    , benchmarkTargets = []
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
        ( "Give extra options to benchmark executables "
            ++ "(split on spaces, use \"\" to prevent splitting; "
            ++ "name templates can use $pkgid, $compiler, "
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
        ( "Give extra option to benchmark executables "
            ++ "(passed directly as a single argument; "
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
