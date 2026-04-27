{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Distribution.Types.TestSuite
  ( TestSuite
  , TestSuiteWith (..)
  , emptyTestSuite
  , testType
  , testModules
  , testModulesAutogen
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.BuildInfo
import Distribution.Types.TestSuiteInterface
import Distribution.Types.TestType
import Distribution.Types.UnqualComponentName

import Distribution.ModuleName

import qualified Distribution.Types.BuildInfo.Lens as L
import qualified Distribution.Types.Modify as Mod

type TestSuite = TestSuiteWith Mod.HasNoAnn

-- | A \"test-suite\" stanza in a cabal file.
data TestSuiteWith (mod :: Mod.HasAnnotation) = TestSuite
  { testName :: UnqualComponentName
  , testInterface :: TestSuiteInterface
  , testBuildInfo :: BuildInfoWith mod
  , testCodeGenerators :: [String]
  }
deriving instance Generic TestSuite
deriving instance Show TestSuite
deriving instance Read TestSuite
deriving instance Eq TestSuite
deriving instance Ord TestSuite
deriving instance Data TestSuite

deriving instance Show (TestSuiteWith Mod.HasAnn)

instance forall (mod :: Mod.HasAnnotation). L.HasBuildInfoWith mod (TestSuiteWith mod) where
  buildInfo f l = (\x -> l{testBuildInfo = x}) <$> f (testBuildInfo l)

instance Binary TestSuite
instance Structured TestSuite

instance NFData TestSuite where rnf = genericRnf

instance Monoid TestSuite where
  mempty =
    TestSuite
      { testName = mempty
      , testInterface = mempty
      , testBuildInfo = mempty
      , testCodeGenerators = mempty
      }
  mappend = (<>)

instance Semigroup TestSuite where
  a <> b =
    TestSuite
      { testName = combineNames a b testName "test"
      , testInterface = combine testInterface
      , testBuildInfo = combine testBuildInfo
      , testCodeGenerators = combine testCodeGenerators
      }
    where
      combine field = field a `mappend` field b

instance Semigroup (TestSuiteWith Mod.HasAnn) where
  a <> b =
    TestSuite
      { testName = combineNames a b testName "test"
      , testInterface = combine testInterface
      , testBuildInfo = combine testBuildInfo
      , testCodeGenerators = combine testCodeGenerators
      }
    where
      combine field = field a `mappend` field b

instance Monoid (TestSuiteWith Mod.HasAnn) where
  mempty = emptyTestSuite'

emptyTestSuite :: TestSuite
emptyTestSuite = mempty

emptyTestSuite' :: TestSuiteWith Mod.HasAnn
emptyTestSuite' =
  TestSuite
    { testName = mempty
    , testInterface = mempty
    , testBuildInfo = mempty
    , testCodeGenerators = mempty
    }

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
