module Distribution.Types.CommonPackageDescription.Lens (
    CommonPackageDescription,
    HasCommonPackageDescription (..),
    IsPackageDescription (..),
    ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compiler                 (CompilerFlavor)
import Distribution.Package                  (Package)
import Distribution.Types.BuildType          (BuildType)
import Distribution.Types.CommonPackageDescription (CommonPackageDescription)
import Distribution.Types.PackageId          (PackageIdentifier)
import Distribution.Types.SetupBuildInfo     (SetupBuildInfo)
import Distribution.Types.SourceRepo         (SourceRepo)
import Distribution.Version                  (Version, VersionRange)

import qualified Distribution.SPDX as SPDX

import Distribution.Types.BuildInfo.Lens
import Distribution.Types.Library.Lens
import Distribution.Types.Executable.Lens
import Distribution.Types.ForeignLib.Lens
import Distribution.Types.TestSuite.Lens
import Distribution.Types.Benchmark.Lens

import qualified Distribution.Types.CommonPackageDescription as T

-- | Classy lenses for 'BuildInfo'.
class Package a => HasCommonPackageDescription a where
   commonPackageDescription :: Lens' a CommonPackageDescription
  
   package :: Lens' a PackageIdentifier
   package = commonPackageDescription . package
   {-# INLINE package #-}
   
   licenseFiles :: Lens' a [String]
   licenseFiles = commonPackageDescription . licenseFiles
   {-# INLINE licenseFiles #-}
   
   copyright :: Lens' a String
   copyright = commonPackageDescription . copyright
   {-# INLINE copyright #-}
   
   maintainer :: Lens' a String
   maintainer = commonPackageDescription . maintainer
   {-# INLINE maintainer #-}
   
   author :: Lens' a String
   author = commonPackageDescription . author
   {-# INLINE author #-}
   
   stability :: Lens' a String
   stability = commonPackageDescription . stability
   {-# INLINE stability #-}
   
   testedWith :: Lens' a [(CompilerFlavor,VersionRange)]
   testedWith = commonPackageDescription . testedWith
   {-# INLINE testedWith #-}
   
   homepage :: Lens' a String
   homepage = commonPackageDescription . homepage
   {-# INLINE homepage #-}
   
   pkgUrl :: Lens' a String
   pkgUrl = commonPackageDescription . pkgUrl
   {-# INLINE pkgUrl #-}
   
   bugReports :: Lens' a String
   bugReports = commonPackageDescription . bugReports
   {-# INLINE bugReports #-}
   
   sourceRepos :: Lens' a [SourceRepo]
   sourceRepos = commonPackageDescription . sourceRepos
   {-# INLINE sourceRepos #-}
   
   synopsis :: Lens' a String
   synopsis = commonPackageDescription . synopsis
   {-# INLINE synopsis #-}
   
   description :: Lens' a String
   description = commonPackageDescription . description
   {-# INLINE description #-}
   
   category :: Lens' a String
   category = commonPackageDescription . category
   {-# INLINE category #-}
   
   customFieldsPD :: Lens' a [(String,String)]
   customFieldsPD = commonPackageDescription . customFieldsPD
   {-# INLINE customFieldsPD #-}
   
   setupBuildInfo :: Lens' a (Maybe SetupBuildInfo)
   setupBuildInfo = commonPackageDescription . setupBuildInfo
   {-# INLINE setupBuildInfo #-}
   
   dataFiles :: Lens' a [FilePath]
   dataFiles = commonPackageDescription . dataFiles
   {-# INLINE dataFiles #-}
   
   dataDir :: Lens' a FilePath
   dataDir = commonPackageDescription . dataDir
   {-# INLINE dataDir #-}
   
   extraSrcFiles :: Lens' a [String]
   extraSrcFiles = commonPackageDescription . extraSrcFiles
   {-# INLINE extraSrcFiles #-}
   
   extraTmpFiles :: Lens' a [String]
   extraTmpFiles = commonPackageDescription . extraTmpFiles
   {-# INLINE extraTmpFiles #-}
   
   extraDocFiles :: Lens' a [String]
   extraDocFiles = commonPackageDescription . extraDocFiles
   {-# INLINE extraDocFiles #-}

instance HasCommonPackageDescription CommonPackageDescription where
   commonPackageDescription = id
   {-# INLINE commonPackageDescription #-}
   
   package f s = fmap (\x -> s { T.package = x }) (f (T.package s))
   {-# INLINE package #-}
   
   licenseFiles f s = fmap (\x -> s { T.licenseFiles = x }) (f (T.licenseFiles s))
   {-# INLINE licenseFiles #-}
   
   copyright f s = fmap (\x -> s { T.copyright = x }) (f (T.copyright s))
   {-# INLINE copyright #-}
   
   maintainer f s = fmap (\x -> s { T.maintainer = x }) (f (T.maintainer s))
   {-# INLINE maintainer #-}
   
   author f s = fmap (\x -> s { T.author = x }) (f (T.author s))
   {-# INLINE author #-}
   
   stability f s = fmap (\x -> s { T.stability = x }) (f (T.stability s))
   {-# INLINE stability #-}
   
   testedWith f s = fmap (\x -> s { T.testedWith = x }) (f (T.testedWith s))
   {-# INLINE testedWith #-}
   
   homepage f s = fmap (\x -> s { T.homepage = x }) (f (T.homepage s))
   {-# INLINE homepage #-}
   
   pkgUrl f s = fmap (\x -> s { T.pkgUrl = x }) (f (T.pkgUrl s))
   {-# INLINE pkgUrl #-}
   
   bugReports f s = fmap (\x -> s { T.bugReports = x }) (f (T.bugReports s))
   {-# INLINE bugReports #-}
   
   sourceRepos f s = fmap (\x -> s { T.sourceRepos = x }) (f (T.sourceRepos s))
   {-# INLINE sourceRepos #-}
   
   synopsis f s = fmap (\x -> s { T.synopsis = x }) (f (T.synopsis s))
   {-# INLINE synopsis #-}
   
   description f s = fmap (\x -> s { T.description = x }) (f (T.description s))
   {-# INLINE description #-}
   
   category f s = fmap (\x -> s { T.category = x }) (f (T.category s))
   {-# INLINE category #-}
   
   customFieldsPD f s = fmap (\x -> s { T.customFieldsPD = x }) (f (T.customFieldsPD s))
   {-# INLINE customFieldsPD #-}
   
   setupBuildInfo f s = fmap (\x -> s { T.setupBuildInfo = x }) (f (T.setupBuildInfo s))
   {-# INLINE setupBuildInfo #-}
   
   dataFiles f s = fmap (\x -> s { T.dataFiles = x }) (f (T.dataFiles s))
   {-# INLINE dataFiles #-}
   
   dataDir f s = fmap (\x -> s { T.dataDir = x }) (f (T.dataDir s))
   {-# INLINE dataDir #-}
   
   extraSrcFiles f s = fmap (\x -> s { T.extraSrcFiles = x }) (f (T.extraSrcFiles s))
   {-# INLINE extraSrcFiles #-}
   
   extraTmpFiles f s = fmap (\x -> s { T.extraTmpFiles = x }) (f (T.extraTmpFiles s))
   {-# INLINE extraTmpFiles #-}
   
   extraDocFiles f s = fmap (\x -> s { T.extraDocFiles = x }) (f (T.extraDocFiles s))
   {-# INLINE extraDocFiles #-}

-- | Accounts for the fields shared between 'PackageDescription' and
-- 'GenericPackageDescription' that cannot go in
-- 'CommonPackageDescription' because they have different types.
class ( HasCommonPackageDescription a
      , HasBuildInfos a
      , HasLibraries a
      , HasExecutables a
      , HasForeignLibs a
      , HasTestSuites a
      , HasBenchmarks a
      ) => IsPackageDescription a where
   lensSpecVersion :: Lens' a Version
   lensLicense :: Lens' a SPDX.License
   lensBuildType :: Lens' a BuildType

   traversePublicLib :: Traversal' a Library
   traverseSubLibs :: Traversal' a Library
