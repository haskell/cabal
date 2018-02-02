{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Distribution.Types.PackageDescription.Lens (
    PackageDescription,
    module Distribution.Types.PackageDescription.Lens,
    ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

-- lens
import Distribution.Types.BuildInfo.Lens
import Distribution.Types.CommonPackageDescription.Lens

import Distribution.Types.Benchmark           (Benchmark)
import Distribution.Types.BuildInfo           (BuildInfo)
import Distribution.Types.BuildType           (BuildType)
import Distribution.Types.Executable          (Executable)
import Distribution.Types.ForeignLib          (ForeignLib)
import Distribution.Types.Library             (Library)
import Distribution.Types.PackageDescription
  (PackageDescription (PackageDescription))
import Distribution.Types.TestSuite           (TestSuite)
import Distribution.Version                   (Version)

import qualified Distribution.SPDX                     as SPDX
import qualified Distribution.Types.PackageDescription as T

specVersion :: Lens' PackageDescription Version
specVersion = lensSpecVersion
{-# INLINE specVersion #-}

license :: Lens' PackageDescription SPDX.License
license = lensLicense
{-# INLINE license #-}

buildType :: Lens' PackageDescription BuildType
buildType = lensBuildType
{-# INLINE buildType #-}

library :: Lens' PackageDescription (Maybe Library)
library f s = fmap (\x -> s { T.library = x }) (f (T.library s))
{-# INLINE library #-}

subLibraries :: Lens' PackageDescription [Library]
subLibraries f s = fmap (\x -> s { T.subLibraries = x }) (f (T.subLibraries s))
{-# INLINE subLibraries #-}

executables :: Lens' PackageDescription [Executable]
executables f s = fmap (\x -> s { T.executables = x }) (f (T.executables s))
{-# INLINE executables #-}

foreignLibs :: Lens' PackageDescription [ForeignLib]
foreignLibs f s = fmap (\x -> s { T.foreignLibs = x }) (f (T.foreignLibs s))
{-# INLINE foreignLibs #-}

testSuites :: Lens' PackageDescription [TestSuite]
testSuites f s = fmap (\x -> s { T.testSuites = x }) (f (T.testSuites s))
{-# INLINE testSuites #-}

benchmarks :: Lens' PackageDescription [Benchmark]
benchmarks f s = fmap (\x -> s { T.benchmarks = x }) (f (T.benchmarks s))
{-# INLINE benchmarks #-}

-------------------------------------------------------------------------------
-- BuildInfos
-------------------------------------------------------------------------------

pdBuildInfos :: Traversal' PackageDescription BuildInfo
pdBuildInfos f (PackageDescription a1 a2 a3 a4 x1 x2 x3 x4 x5 x6) =
    PackageDescription a1 a2 a3 a4
        <$> (traverse . buildInfo) f x1
        <*> (traverse . buildInfo) f x2
        <*> (traverse . buildInfo) f x3
        <*> (traverse . buildInfo) f x4
        <*> (traverse . buildInfo) f x5
        <*> (traverse . buildInfo) f x6
