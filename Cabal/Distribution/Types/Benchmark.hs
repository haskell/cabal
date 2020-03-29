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
import Distribution.Types.CommonStanzaImports
import Distribution.Types.BenchmarkType
import Distribution.Types.BenchmarkInterface
import Distribution.Types.UnqualComponentName

import Distribution.ModuleName

import qualified Distribution.Types.BuildInfo.Lens as L
import qualified Distribution.Types.CommonStanzaImports.Lens as L

-- | A \"benchmark\" stanza in a cabal file.
--
data Benchmark = Benchmark {
        benchmarkName      :: UnqualComponentName,
        benchmarkImports   :: CommonStanzaImports,
        benchmarkInterface :: BenchmarkInterface,
        benchmarkBuildInfo :: BuildInfo
    }
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary Benchmark
instance Structured Benchmark
instance NFData Benchmark where rnf = genericRnf

instance L.HasCommonStanzaImports Benchmark where
    commonStanzaImports f (Benchmark x1 x2 x3 x4) = fmap (\i1 -> Benchmark x1 i1 x3 x4) (f x2)

instance L.HasBuildInfo Benchmark where
    buildInfo f (Benchmark x1 x2 x3 x4) = fmap (\y1 -> Benchmark x1 x2 x3 y1) (f x4)

instance Monoid Benchmark where
    mempty = Benchmark {
        benchmarkName      = mempty,
        benchmarkImports   = mempty,
        benchmarkInterface = mempty,
        benchmarkBuildInfo = mempty
    }
    mappend = (<>)

instance Semigroup Benchmark where
    a <> b = Benchmark {
        benchmarkName      = combine' benchmarkName,
        benchmarkImports   = combine  benchmarkImports,
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
