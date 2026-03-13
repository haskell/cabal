{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Distribution.Types.Benchmark
  ( Benchmark
  , BenchmarkWith (..)
  , emptyBenchmark
  , benchmarkType
  , benchmarkModules
  , benchmarkModulesAutogen
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Annotation
import Distribution.Types.BenchmarkInterface
import Distribution.Types.BenchmarkType
import Distribution.Types.BuildInfo
import Distribution.Types.UnqualComponentName

import Distribution.ModuleName

import qualified Distribution.Types.BuildInfo.Lens as L

type Benchmark = BenchmarkWith Abst

-- | A \"benchmark\" stanza in a cabal file.
data BenchmarkWith (mod :: ParsingPhase) = Benchmark
  { benchmarkName :: UnqualComponentName
  , benchmarkInterface :: BenchmarkInterface
  , benchmarkBuildInfo :: BuildInfoWith mod
  }

deriving instance Generic Benchmark
deriving instance Show Benchmark
deriving instance Read Benchmark
deriving instance Eq Benchmark
deriving instance Ord Benchmark
deriving instance Data Benchmark

deriving instance Show (BenchmarkWith Conc)

instance Binary Benchmark
instance Structured Benchmark
instance NFData Benchmark where rnf = genericRnf

instance forall (mod :: ParsingPhase). L.HasBuildInfoWith mod (BenchmarkWith mod) where
  buildInfo f (Benchmark x1 x2 x3) = fmap (\y1 -> Benchmark x1 x2 y1) (f x3)

instance Monoid Benchmark where
  mempty =
    Benchmark
      { benchmarkName = mempty
      , benchmarkInterface = mempty
      , benchmarkBuildInfo = mempty
      }
  mappend = (<>)

instance Monoid (BenchmarkWith Conc) where
  mempty =
    Benchmark
      { benchmarkName = mempty
      , benchmarkInterface = mempty
      , benchmarkBuildInfo = mempty
      }
  mappend = (<>)

instance Semigroup Benchmark where
  a <> b =
    Benchmark
      { benchmarkName = combineNames a b benchmarkName "benchmark"
      , benchmarkInterface = combine benchmarkInterface
      , benchmarkBuildInfo = combine benchmarkBuildInfo
      }
    where
      combine field = field a `mappend` field b

instance Semigroup (BenchmarkWith Conc) where
  a <> b =
    Benchmark
      { benchmarkName = combineNames a b benchmarkName "benchmark"
      , benchmarkInterface = combine benchmarkInterface
      , benchmarkBuildInfo = combine benchmarkBuildInfo
      }
    where
      combine field = field a `mappend` field b

emptyBenchmark :: Benchmark
emptyBenchmark = mempty

benchmarkType :: Benchmark -> BenchmarkType
benchmarkType benchmark = case benchmarkInterface benchmark of
  BenchmarkExeV10 ver _ -> BenchmarkTypeExe ver
  BenchmarkUnsupported benchmarktype -> benchmarktype

-- | Get all the module names from a benchmark.
benchmarkModules :: Benchmark -> [ModuleName]
benchmarkModules benchmark = otherModules (benchmarkBuildInfo benchmark)

-- | Get all the auto generated module names from a benchmark.
-- This are a subset of 'benchmarkModules'.
benchmarkModulesAutogen :: Benchmark -> [ModuleName]
benchmarkModulesAutogen benchmark = autogenModules (benchmarkBuildInfo benchmark)
