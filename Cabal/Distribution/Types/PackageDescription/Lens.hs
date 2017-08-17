module Distribution.Types.PackageDescription.Lens (
    PackageDescription,
    module Distribution.Types.PackageDescription.Lens,
    ) where

import Prelude()
import Distribution.Compat.Prelude
import Distribution.Compat.Lens

import Distribution.Types.PackageDescription (PackageDescription)
import qualified Distribution.Types.PackageDescription as T

import Distribution.Types.SetupBuildInfo (SetupBuildInfo)
import Distribution.Types.SourceRepo (SourceRepo)

customFieldsPD :: Lens' PackageDescription [(String,String)]
customFieldsPD f pd = fmap (\x -> pd { T.customFieldsPD = x }) (f (T.customFieldsPD pd))

description :: Lens' PackageDescription String
description f pd = fmap (\x -> pd { T.description = x }) (f (T.description pd))

synopsis :: Lens' PackageDescription String
synopsis f pd = fmap (\x -> pd { T.synopsis = x }) (f (T.synopsis pd))

maintainer :: Lens' PackageDescription String
maintainer f pd = fmap (\x -> pd { T.maintainer = x }) (f (T.maintainer pd))

setupBuildInfo :: Lens' PackageDescription (Maybe SetupBuildInfo)
setupBuildInfo f pd = fmap (\x -> pd { T.setupBuildInfo = x }) (f (T.setupBuildInfo pd))

sourceRepos :: Lens' PackageDescription [SourceRepo]
sourceRepos f pd = fmap (\x -> pd { T.sourceRepos = x }) (f (T.sourceRepos pd))
