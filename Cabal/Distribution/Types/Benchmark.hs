{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Benchmark (
    Benchmark(..),
    emptyBenchmark,
    benchmarkType,
    benchmarkModules,
    benchmarkModulesAutogen
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.BuildInfo
import Distribution.Types.BenchmarkType
import Distribution.Types.BenchmarkInterface

import Distribution.ModuleName
import Distribution.Package

-- | A \"benchmark\" stanza in a cabal file.
--
data Benchmark = Benchmark {
        benchmarkName      :: UnqualComponentName,
        benchmarkInterface :: BenchmarkInterface,
        benchmarkBuildInfo :: BuildInfo
    }
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary Benchmark

instance Monoid Benchmark where
    mempty = Benchmark {
        benchmarkName      = mempty,
        benchmarkInterface = mempty,
        benchmarkBuildInfo = mempty
    }
    mappend = (<>)

instance Semigroup Benchmark where
    a <> b = Benchmark {
        benchmarkName      = combine' benchmarkName,
        benchmarkInterface = combine  benchmarkInterface,
        benchmarkBuildInfo = combine  benchmarkBuildInfo
    }
        where combine  field = field a `mappend` field b
              combine' field = case ( unUnqualComponentName $ field a
                                    , unUnqualComponentName $ field b) of
                        ("", _) -> field b
                        (_, "") -> field a
                        (x, y) -> error $ "Ambiguous values for test field: '"
                            ++ x ++ "' and '" ++ y ++ "'"

emptyBenchmark :: Benchmark
emptyBenchmark = mempty

benchmarkType :: Benchmark -> BenchmarkType
benchmarkType benchmark = case benchmarkInterface benchmark of
  BenchmarkExeV10 ver _              -> BenchmarkTypeExe ver
  BenchmarkUnsupported benchmarktype -> benchmarktype

-- | Get all the module names from a benchmark.
benchmarkModules :: Benchmark -> [ModuleName]
benchmarkModules benchmark = otherModules (benchmarkBuildInfo benchmark)

-- | Get all the auto generated module names from a benchmark.
-- This are a subset of 'benchmarkModules'.
benchmarkModulesAutogen :: Benchmark -> [ModuleName]
benchmarkModulesAutogen benchmark = autogenModules (benchmarkBuildInfo benchmark)
