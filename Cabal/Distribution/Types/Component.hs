{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Component (
    Component(..),
    foldComponent,
    componentBuildInfo,
    componentBuildable,
    componentName,
) where

import Distribution.Types.Library
import Distribution.Types.Executable
import Distribution.Types.TestSuite
import Distribution.Types.Benchmark

import Distribution.Types.ComponentName
import Distribution.Types.BuildInfo

data Component = CLib   Library
               | CExe   Executable
               | CTest  TestSuite
               | CBench Benchmark
               deriving (Show, Eq, Read)

foldComponent :: (Library -> a)
              -> (Executable -> a)
              -> (TestSuite -> a)
              -> (Benchmark -> a)
              -> Component
              -> a
foldComponent f _ _ _ (CLib   lib) = f lib
foldComponent _ f _ _ (CExe   exe) = f exe
foldComponent _ _ f _ (CTest  tst) = f tst
foldComponent _ _ _ f (CBench bch) = f bch

componentBuildInfo :: Component -> BuildInfo
componentBuildInfo =
  foldComponent libBuildInfo buildInfo testBuildInfo benchmarkBuildInfo

-- | Is a component buildable (i.e., not marked with @buildable: False@)?
-- See also this note in
-- "Distribution.Types.ComponentEnabledSpec#buildable_vs_enabled_components".
--
-- @since 1.26.0.0
--
componentBuildable :: Component -> Bool
componentBuildable = buildable . componentBuildInfo

componentName :: Component -> ComponentName
componentName =
  foldComponent getLibName
                (CExeName . exeName)
                (CTestName . testName)
                (CBenchName . benchmarkName)
 where
  getLibName lib = case libName lib of
                    Nothing -> CLibName
                    Just n -> CSubLibName n
