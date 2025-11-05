{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.TestSuite
  ( TestSuite (..)
  , emptyTestSuite
  , testType
  , testModules
  , testModulesAutogen
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.ConfVar
import Distribution.Types.Dependency
import Distribution.Types.TestSuiteInterface
import Distribution.Types.TestType
import Distribution.Types.UnqualComponentName

import Distribution.ModuleName

import qualified Distribution.Types.BuildInfo.Lens as L

-- | A \"test-suite\" stanza in a cabal file.
data TestSuite = TestSuite
  { testName :: UnqualComponentName
  , testSuiteImports :: [(ImportName, CondTree ConfVar [Dependency] BuildInfo)]
  , testInterface :: TestSuiteInterface
  , testBuildInfo :: BuildInfo
  , testCodeGenerators :: [String]
  }
  -- TODO(leana8959): instances
  deriving (Generic, Show {- Read, -}, Eq {- Ord, -}, Data)

instance L.HasBuildInfo TestSuite where
  buildInfo f l = (\x -> l{testBuildInfo = x}) <$> f (testBuildInfo l)

instance Binary TestSuite
instance Structured TestSuite

instance NFData TestSuite where rnf = genericRnf

instance Monoid TestSuite where
  mempty =
    TestSuite
      { testName = mempty
      , testSuiteImports = mempty
      , testInterface = mempty
      , testBuildInfo = mempty
      , testCodeGenerators = mempty
      }
  mappend = (<>)

instance Semigroup TestSuite where
  a <> b =
    TestSuite
      { testName = combineNames a b testName "test"
      , testSuiteImports = combine testSuiteImports
      , testInterface = combine testInterface
      , testBuildInfo = combine testBuildInfo
      , testCodeGenerators = combine testCodeGenerators
      }
    where
      combine field = field a `mappend` field b

emptyTestSuite :: TestSuite
emptyTestSuite = mempty

testType :: TestSuite -> TestType
testType test = case testInterface test of
  TestSuiteExeV10 ver _ -> TestTypeExe ver
  TestSuiteLibV09 ver _ -> TestTypeLib ver
  TestSuiteUnsupported testtype -> testtype

-- | Get all the module names from a test suite.
testModules :: TestSuite -> [ModuleName]
testModules test =
  ( case testInterface test of
      TestSuiteLibV09 _ m -> [m]
      _ -> []
  )
    ++ otherModules (testBuildInfo test)

-- | Get all the auto generated module names from a test suite.
-- This are a subset of 'testModules'.
testModulesAutogen :: TestSuite -> [ModuleName]
testModulesAutogen test = autogenModules (testBuildInfo test)
