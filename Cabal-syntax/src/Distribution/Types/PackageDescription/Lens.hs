{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Types.PackageDescription.Lens
  ( PackageDescription
  , module Distribution.Types.PackageDescription.Lens
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Compiler (CompilerFlavor)
import Distribution.License (License)
import Distribution.ModuleName (ModuleName)
import Distribution.Types.Benchmark (Benchmark, benchmarkModules)
import Distribution.Types.Benchmark.Lens (benchmarkBuildInfo, benchmarkName)
import Distribution.Types.BuildInfo (BuildInfo)
import Distribution.Types.BuildType (BuildType)
import Distribution.Types.ComponentName (ComponentName (..))
import Distribution.Types.Executable (Executable, exeModules)
import Distribution.Types.Executable.Lens (exeBuildInfo, exeName)
import Distribution.Types.ForeignLib (ForeignLib, foreignLibModules)
import Distribution.Types.ForeignLib.Lens (foreignLibBuildInfo, foreignLibName)
import Distribution.Types.Library (Library, explicitLibModules)
import Distribution.Types.Library.Lens (libBuildInfo, libName)
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Types.PackageId (PackageIdentifier)
import Distribution.Types.SetupBuildInfo (SetupBuildInfo)
import Distribution.Types.SourceRepo (SourceRepo)
import Distribution.Types.TestSuite (TestSuite, testModules)
import Distribution.Types.TestSuite.Lens (testBuildInfo, testName)
import Distribution.Utils.Path
import Distribution.Utils.ShortText (ShortText)
import Distribution.Version (VersionRange)

import qualified Distribution.SPDX as SPDX
import qualified Distribution.Types.PackageDescription as T

package :: Lens' PackageDescription PackageIdentifier
package f s = fmap (\x -> s{T.package = x}) (f (T.package s))
{-# INLINE package #-}

licenseRaw :: Lens' PackageDescription (Either SPDX.License License)
licenseRaw f s = fmap (\x -> s{T.licenseRaw = x}) (f (T.licenseRaw s))
{-# INLINE licenseRaw #-}

licenseFiles :: Lens' PackageDescription [RelativePath Pkg File]
licenseFiles f s = fmap (\x -> s{T.licenseFiles = x}) (f (T.licenseFiles s))
{-# INLINE licenseFiles #-}

copyright :: Lens' PackageDescription ShortText
copyright f s = fmap (\x -> s{T.copyright = x}) (f (T.copyright s))
{-# INLINE copyright #-}

maintainer :: Lens' PackageDescription ShortText
maintainer f s = fmap (\x -> s{T.maintainer = x}) (f (T.maintainer s))
{-# INLINE maintainer #-}

author :: Lens' PackageDescription ShortText
author f s = fmap (\x -> s{T.author = x}) (f (T.author s))
{-# INLINE author #-}

stability :: Lens' PackageDescription ShortText
stability f s = fmap (\x -> s{T.stability = x}) (f (T.stability s))
{-# INLINE stability #-}

testedWith :: Lens' PackageDescription [(CompilerFlavor, VersionRange)]
testedWith f s = fmap (\x -> s{T.testedWith = x}) (f (T.testedWith s))
{-# INLINE testedWith #-}

homepage :: Lens' PackageDescription ShortText
homepage f s = fmap (\x -> s{T.homepage = x}) (f (T.homepage s))
{-# INLINE homepage #-}

pkgUrl :: Lens' PackageDescription ShortText
pkgUrl f s = fmap (\x -> s{T.pkgUrl = x}) (f (T.pkgUrl s))
{-# INLINE pkgUrl #-}

bugReports :: Lens' PackageDescription ShortText
bugReports f s = fmap (\x -> s{T.bugReports = x}) (f (T.bugReports s))
{-# INLINE bugReports #-}

sourceRepos :: Lens' PackageDescription [SourceRepo]
sourceRepos f s = fmap (\x -> s{T.sourceRepos = x}) (f (T.sourceRepos s))
{-# INLINE sourceRepos #-}

synopsis :: Lens' PackageDescription ShortText
synopsis f s = fmap (\x -> s{T.synopsis = x}) (f (T.synopsis s))
{-# INLINE synopsis #-}

description :: Lens' PackageDescription ShortText
description f s = fmap (\x -> s{T.description = x}) (f (T.description s))
{-# INLINE description #-}

category :: Lens' PackageDescription ShortText
category f s = fmap (\x -> s{T.category = x}) (f (T.category s))
{-# INLINE category #-}

customFieldsPD :: Lens' PackageDescription [(String, String)]
customFieldsPD f s = fmap (\x -> s{T.customFieldsPD = x}) (f (T.customFieldsPD s))
{-# INLINE customFieldsPD #-}

specVersion :: Lens' PackageDescription CabalSpecVersion
specVersion f s = fmap (\x -> s{T.specVersion = x}) (f (T.specVersion s))
{-# INLINE specVersion #-}

buildTypeRaw :: Lens' PackageDescription (Maybe BuildType)
buildTypeRaw f s = fmap (\x -> s{T.buildTypeRaw = x}) (f (T.buildTypeRaw s))
{-# INLINE buildTypeRaw #-}

setupBuildInfo :: Lens' PackageDescription (Maybe SetupBuildInfo)
setupBuildInfo f s = fmap (\x -> s{T.setupBuildInfo = x}) (f (T.setupBuildInfo s))
{-# INLINE setupBuildInfo #-}

library :: Lens' PackageDescription (Maybe Library)
library f s = fmap (\x -> s{T.library = x}) (f (T.library s))
{-# INLINE library #-}

subLibraries :: Lens' PackageDescription [Library]
subLibraries f s = fmap (\x -> s{T.subLibraries = x}) (f (T.subLibraries s))
{-# INLINE subLibraries #-}

executables :: Lens' PackageDescription [Executable]
executables f s = fmap (\x -> s{T.executables = x}) (f (T.executables s))
{-# INLINE executables #-}

foreignLibs :: Lens' PackageDescription [ForeignLib]
foreignLibs f s = fmap (\x -> s{T.foreignLibs = x}) (f (T.foreignLibs s))
{-# INLINE foreignLibs #-}

testSuites :: Lens' PackageDescription [TestSuite]
testSuites f s = fmap (\x -> s{T.testSuites = x}) (f (T.testSuites s))
{-# INLINE testSuites #-}

benchmarks :: Lens' PackageDescription [Benchmark]
benchmarks f s = fmap (\x -> s{T.benchmarks = x}) (f (T.benchmarks s))
{-# INLINE benchmarks #-}

dataFiles :: Lens' PackageDescription [RelativePath DataDir File]
dataFiles f s = fmap (\x -> s{T.dataFiles = x}) (f (T.dataFiles s))
{-# INLINE dataFiles #-}

dataDir :: Lens' PackageDescription (SymbolicPath Pkg (Dir DataDir))
dataDir f s = fmap (\x -> s{T.dataDir = x}) (f (T.dataDir s))
{-# INLINE dataDir #-}

extraSrcFiles :: Lens' PackageDescription [RelativePath Pkg File]
extraSrcFiles f s = fmap (\x -> s{T.extraSrcFiles = x}) (f (T.extraSrcFiles s))
{-# INLINE extraSrcFiles #-}

extraTmpFiles :: Lens' PackageDescription [RelativePath Pkg File]
extraTmpFiles f s = fmap (\x -> s{T.extraTmpFiles = x}) (f (T.extraTmpFiles s))
{-# INLINE extraTmpFiles #-}

extraDocFiles :: Lens' PackageDescription [RelativePath Pkg File]
extraDocFiles f s = fmap (\x -> s{T.extraDocFiles = x}) (f (T.extraDocFiles s))
{-# INLINE extraDocFiles #-}

-- | @since 3.0.0.0
allLibraries :: Traversal' PackageDescription Library
allLibraries f pd = mk <$> traverse f (T.library pd) <*> traverse f (T.subLibraries pd)
  where
    mk l ls = pd{T.library = l, T.subLibraries = ls}

-- | @since 2.4
componentModules :: Monoid r => ComponentName -> Getting r PackageDescription [ModuleName]
componentModules cname = case cname of
  CLibName name ->
    componentModules' name allLibraries libName explicitLibModules
  CFLibName name ->
    componentModules' name (foreignLibs . traverse) foreignLibName foreignLibModules
  CExeName name ->
    componentModules' name (executables . traverse) exeName exeModules
  CTestName name ->
    componentModules' name (testSuites . traverse) testName testModules
  CBenchName name ->
    componentModules' name (benchmarks . traverse) benchmarkName benchmarkModules
  where
    componentModules'
      :: (Eq name, Monoid r)
      => name
      -> Traversal' PackageDescription a
      -> Lens' a name
      -> (a -> [ModuleName])
      -> Getting r PackageDescription [ModuleName]
    componentModules' name pdL nameL modules =
      pdL
        . filtered ((== name) . view nameL)
        . getting modules

    filtered :: (a -> Bool) -> Traversal' a a
    filtered p f s = if p s then f s else pure s

-- | @since 2.4
componentBuildInfo :: ComponentName -> Traversal' PackageDescription BuildInfo
componentBuildInfo cname = case cname of
  CLibName name ->
    componentBuildInfo' name allLibraries libName libBuildInfo
  CFLibName name ->
    componentBuildInfo' name (foreignLibs . traverse) foreignLibName foreignLibBuildInfo
  CExeName name ->
    componentBuildInfo' name (executables . traverse) exeName exeBuildInfo
  CTestName name ->
    componentBuildInfo' name (testSuites . traverse) testName testBuildInfo
  CBenchName name ->
    componentBuildInfo' name (benchmarks . traverse) benchmarkName benchmarkBuildInfo
  where
    componentBuildInfo'
      :: Eq name
      => name
      -> Traversal' PackageDescription a
      -> Lens' a name
      -> Traversal' a BuildInfo
      -> Traversal' PackageDescription BuildInfo
    componentBuildInfo' name pdL nameL biL =
      pdL
        . filtered ((== name) . view nameL)
        . biL

    filtered :: (a -> Bool) -> Traversal' a a
    filtered p f s = if p s then f s else pure s
