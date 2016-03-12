{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.OptionalStanza
    ( OptionalStanza(..)
    , enableStanzas
    ) where

import Distribution.PackageDescription
    ( GenericPackageDescription(..)
    , benchmarkEnabled
    , testEnabled)
import Distribution.PackageDescription.Configuration
    ( mapTreeData
    )
import Distribution.Compat.Binary (Binary(..))
import GHC.Generics (Generic)

data OptionalStanza
    = TestStanzas
    | BenchStanzas
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance Binary OptionalStanza

enableStanzas
    :: [OptionalStanza]
    -> GenericPackageDescription
    -> GenericPackageDescription
enableStanzas stanzas gpkg = gpkg
    { condBenchmarks = flagBenchmarks $ condBenchmarks gpkg
    , condTestSuites = flagTests $ condTestSuites gpkg
    }
  where
    enableTest t = t { testEnabled = TestStanzas `elem` stanzas }
    enableBenchmark bm = bm { benchmarkEnabled = BenchStanzas `elem` stanzas }
    flagBenchmarks = map (\(n, bm) -> (n, mapTreeData enableBenchmark bm))
    flagTests = map (\(n, t) -> (n, mapTreeData enableTest t))
