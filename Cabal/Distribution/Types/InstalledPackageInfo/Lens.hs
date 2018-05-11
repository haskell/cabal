module Distribution.Types.InstalledPackageInfo.Lens (
    InstalledPackageInfo,
    module Distribution.Types.InstalledPackageInfo.Lens
    ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Backpack                   (OpenModule)
import Distribution.License                    (License)
import Distribution.ModuleName                 (ModuleName)
import Distribution.Package                    (AbiHash, ComponentId, PackageIdentifier, UnitId)
import Distribution.Types.InstalledPackageInfo (AbiDependency, ExposedModule, InstalledPackageInfo)
import Distribution.Types.UnqualComponentName  (UnqualComponentName)

import qualified Distribution.SPDX                       as SPDX
import qualified Distribution.Types.InstalledPackageInfo as T

sourcePackageId :: Lens' InstalledPackageInfo PackageIdentifier
sourcePackageId f s = fmap (\x -> s { T.sourcePackageId = x }) (f (T.sourcePackageId s))
{-# INLINE sourcePackageId #-}

installedUnitId :: Lens' InstalledPackageInfo UnitId
installedUnitId f s = fmap (\x -> s { T.installedUnitId = x }) (f (T.installedUnitId s))
{-# INLINE installedUnitId #-}

installedComponentId_ :: Lens' InstalledPackageInfo ComponentId
installedComponentId_ f s = fmap (\x -> s { T.installedComponentId_ = x }) (f (T.installedComponentId_ s))
{-# INLINE installedComponentId_ #-}

instantiatedWith :: Lens' InstalledPackageInfo [(ModuleName,OpenModule)]
instantiatedWith f s = fmap (\x -> s { T.instantiatedWith = x }) (f (T.instantiatedWith s))
{-# INLINE instantiatedWith #-}

sourceLibName :: Lens' InstalledPackageInfo (Maybe UnqualComponentName)
sourceLibName f s = fmap (\x -> s { T.sourceLibName = x }) (f (T.sourceLibName s))
{-# INLINE sourceLibName #-}

compatPackageKey :: Lens' InstalledPackageInfo String
compatPackageKey f s = fmap (\x -> s { T.compatPackageKey = x }) (f (T.compatPackageKey s))
{-# INLINE compatPackageKey #-}

license :: Lens' InstalledPackageInfo (Either SPDX.License License)
license f s = fmap (\x -> s { T.license = x }) (f (T.license s))
{-# INLINE license #-}

copyright :: Lens' InstalledPackageInfo String
copyright f s = fmap (\x -> s { T.copyright = x }) (f (T.copyright s))
{-# INLINE copyright #-}

maintainer :: Lens' InstalledPackageInfo String
maintainer f s = fmap (\x -> s { T.maintainer = x }) (f (T.maintainer s))
{-# INLINE maintainer #-}

author :: Lens' InstalledPackageInfo String
author f s = fmap (\x -> s { T.author = x }) (f (T.author s))
{-# INLINE author #-}

stability :: Lens' InstalledPackageInfo String
stability f s = fmap (\x -> s { T.stability = x }) (f (T.stability s))
{-# INLINE stability #-}

homepage :: Lens' InstalledPackageInfo String
homepage f s = fmap (\x -> s { T.homepage = x }) (f (T.homepage s))
{-# INLINE homepage #-}

pkgUrl :: Lens' InstalledPackageInfo String
pkgUrl f s = fmap (\x -> s { T.pkgUrl = x }) (f (T.pkgUrl s))
{-# INLINE pkgUrl #-}

synopsis :: Lens' InstalledPackageInfo String
synopsis f s = fmap (\x -> s { T.synopsis = x }) (f (T.synopsis s))
{-# INLINE synopsis #-}

description :: Lens' InstalledPackageInfo String
description f s = fmap (\x -> s { T.description = x }) (f (T.description s))
{-# INLINE description #-}

category :: Lens' InstalledPackageInfo String
category f s = fmap (\x -> s { T.category = x }) (f (T.category s))
{-# INLINE category #-}

abiHash :: Lens' InstalledPackageInfo AbiHash
abiHash f s = fmap (\x -> s { T.abiHash = x }) (f (T.abiHash s))
{-# INLINE abiHash #-}

indefinite :: Lens' InstalledPackageInfo Bool
indefinite f s = fmap (\x -> s { T.indefinite = x }) (f (T.indefinite s))
{-# INLINE indefinite #-}

exposed :: Lens' InstalledPackageInfo Bool
exposed f s = fmap (\x -> s { T.exposed = x }) (f (T.exposed s))
{-# INLINE exposed #-}

exposedModules :: Lens' InstalledPackageInfo [ExposedModule]
exposedModules f s = fmap (\x -> s { T.exposedModules = x }) (f (T.exposedModules s))
{-# INLINE exposedModules #-}

hiddenModules :: Lens' InstalledPackageInfo [ModuleName]
hiddenModules f s = fmap (\x -> s { T.hiddenModules = x }) (f (T.hiddenModules s))
{-# INLINE hiddenModules #-}

trusted :: Lens' InstalledPackageInfo Bool
trusted f s = fmap (\x -> s { T.trusted = x }) (f (T.trusted s))
{-# INLINE trusted #-}

importDirs :: Lens' InstalledPackageInfo [FilePath]
importDirs f s = fmap (\x -> s { T.importDirs = x }) (f (T.importDirs s))
{-# INLINE importDirs #-}

libraryDirs :: Lens' InstalledPackageInfo [FilePath]
libraryDirs f s = fmap (\x -> s { T.libraryDirs = x }) (f (T.libraryDirs s))
{-# INLINE libraryDirs #-}

libraryDynDirs :: Lens' InstalledPackageInfo [FilePath]
libraryDynDirs f s = fmap (\x -> s { T.libraryDynDirs = x }) (f (T.libraryDynDirs s))
{-# INLINE libraryDynDirs #-}

dataDir :: Lens' InstalledPackageInfo FilePath
dataDir f s = fmap (\x -> s { T.dataDir = x }) (f (T.dataDir s))
{-# INLINE dataDir #-}

hsLibraries :: Lens' InstalledPackageInfo [String]
hsLibraries f s = fmap (\x -> s { T.hsLibraries = x }) (f (T.hsLibraries s))
{-# INLINE hsLibraries #-}

extraLibraries :: Lens' InstalledPackageInfo [String]
extraLibraries f s = fmap (\x -> s { T.extraLibraries = x }) (f (T.extraLibraries s))
{-# INLINE extraLibraries #-}

extraGHCiLibraries :: Lens' InstalledPackageInfo [String]
extraGHCiLibraries f s = fmap (\x -> s { T.extraGHCiLibraries = x }) (f (T.extraGHCiLibraries s))
{-# INLINE extraGHCiLibraries #-}

includeDirs :: Lens' InstalledPackageInfo [FilePath]
includeDirs f s = fmap (\x -> s { T.includeDirs = x }) (f (T.includeDirs s))
{-# INLINE includeDirs #-}

includes :: Lens' InstalledPackageInfo [String]
includes f s = fmap (\x -> s { T.includes = x }) (f (T.includes s))
{-# INLINE includes #-}

depends :: Lens' InstalledPackageInfo [UnitId]
depends f s = fmap (\x -> s { T.depends = x }) (f (T.depends s))
{-# INLINE depends #-}

abiDepends :: Lens' InstalledPackageInfo [AbiDependency]
abiDepends f s = fmap (\x -> s { T.abiDepends = x }) (f (T.abiDepends s))
{-# INLINE abiDepends #-}

ccOptions :: Lens' InstalledPackageInfo [String]
ccOptions f s = fmap (\x -> s { T.ccOptions = x }) (f (T.ccOptions s))
{-# INLINE ccOptions #-}

ldOptions :: Lens' InstalledPackageInfo [String]
ldOptions f s = fmap (\x -> s { T.ldOptions = x }) (f (T.ldOptions s))
{-# INLINE ldOptions #-}

frameworkDirs :: Lens' InstalledPackageInfo [FilePath]
frameworkDirs f s = fmap (\x -> s { T.frameworkDirs = x }) (f (T.frameworkDirs s))
{-# INLINE frameworkDirs #-}

frameworks :: Lens' InstalledPackageInfo [String]
frameworks f s = fmap (\x -> s { T.frameworks = x }) (f (T.frameworks s))
{-# INLINE frameworks #-}

haddockInterfaces :: Lens' InstalledPackageInfo [FilePath]
haddockInterfaces f s = fmap (\x -> s { T.haddockInterfaces = x }) (f (T.haddockInterfaces s))
{-# INLINE haddockInterfaces #-}

haddockHTMLs :: Lens' InstalledPackageInfo [FilePath]
haddockHTMLs f s = fmap (\x -> s { T.haddockHTMLs = x }) (f (T.haddockHTMLs s))
{-# INLINE haddockHTMLs #-}

pkgRoot :: Lens' InstalledPackageInfo (Maybe FilePath)
pkgRoot f s = fmap (\x -> s { T.pkgRoot = x }) (f (T.pkgRoot s))
{-# INLINE pkgRoot #-}

