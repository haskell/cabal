{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.BenchmarkInterface
  ( BenchmarkInterface (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.BenchmarkType
import Distribution.Version

-- | The benchmark interfaces that are currently defined.
--
-- More interfaces may be defined in future, either new revisions or
-- totally new interfaces.
data BenchmarkInterface
  = -- | Benchmark interface \"exitcode-stdio-1.0\". The benchmark
    -- takes the form of an executable. It returns a zero exit code
    -- for success, non-zero for failure. The stdout and stderr
    -- channels may be logged. Test tooling may pass command line
    -- arguments and/or connect the stdin channel to the test.
    BenchmarkExeV10 Version FilePath
  | -- | A benchmark that does not conform to one of the above
    -- interfaces for the given reason (e.g. unknown benchmark type).
    BenchmarkUnsupported BenchmarkType
  deriving (Eq, Ord, Generic, Read, Show, Typeable, Data)

instance Binary BenchmarkInterface
instance Structured BenchmarkInterface
instance NFData BenchmarkInterface where rnf = genericRnf

instance Monoid BenchmarkInterface where
  mempty = BenchmarkUnsupported (BenchmarkTypeUnknown mempty nullVersion)
  mappend = (<>)

instance Semigroup BenchmarkInterface where
  a <> (BenchmarkUnsupported _) = a
  _ <> b = b
