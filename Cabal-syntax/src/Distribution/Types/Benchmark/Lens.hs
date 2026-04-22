module Distribution.Types.Benchmark.Lens
  ( Benchmark
  , module Distribution.Types.Benchmark.Lens
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Benchmark (Benchmark, BenchmarkWith)
import Distribution.Types.BenchmarkInterface (BenchmarkInterface)
import Distribution.Types.BuildInfo (BuildInfo, BuildInfoWith)
import Distribution.Types.UnqualComponentName (UnqualComponentName)

import qualified Distribution.Types.Benchmark as T

benchmarkName :: Lens' (BenchmarkWith mod) UnqualComponentName
benchmarkName f s = fmap (\x -> s{T.benchmarkName = x}) (f (T.benchmarkName s))
{-# INLINE benchmarkName #-}

benchmarkInterface :: Lens' (BenchmarkWith mod) BenchmarkInterface
benchmarkInterface f s = fmap (\x -> s{T.benchmarkInterface = x}) (f (T.benchmarkInterface s))
{-# INLINE benchmarkInterface #-}

benchmarkBuildInfo :: Lens' (BenchmarkWith mod) (BuildInfoWith mod)
benchmarkBuildInfo f s = fmap (\x -> s{T.benchmarkBuildInfo = x}) (f (T.benchmarkBuildInfo s))
{-# INLINE benchmarkBuildInfo #-}
