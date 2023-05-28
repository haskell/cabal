{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.TestSuiteInterface
  ( TestSuiteInterface (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName
import Distribution.Types.TestType
import Distribution.Version

-- | The test suite interfaces that are currently defined.
--
-- More interfaces may be defined in future, either new revisions or totally
-- new interfaces.
data TestSuiteInterface
  = -- | Test interface \"exitcode-stdio-1.0\". The test-suite takes the form
    -- of an executable. It returns a zero exit code for success, non-zero for
    -- failure. The stdout and stderr channels may be logged. Test tooling may
    -- pass command line arguments and/or connect the stdin channel to the test.
    TestSuiteExeV10 Version FilePath
  | -- | Test interface \"detailed-0.9\". The test-suite takes the form of a
    -- library containing a designated module that exports \"tests :: [Test]\".
    TestSuiteLibV09 Version ModuleName
  | -- | A test suite that does not conform to one of the above interfaces for
    -- the given reason (e.g. unknown test type).
    TestSuiteUnsupported TestType
  deriving (Eq, Ord, Generic, Read, Show, Typeable, Data)

instance Binary TestSuiteInterface
instance Structured TestSuiteInterface

instance NFData TestSuiteInterface where rnf = genericRnf

instance Monoid TestSuiteInterface where
  mempty = TestSuiteUnsupported (TestTypeUnknown mempty nullVersion)
  mappend = (<>)

instance Semigroup TestSuiteInterface where
  a <> (TestSuiteUnsupported _) = a
  _ <> b = b
