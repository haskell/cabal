{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.DefaultBounds
  ( DefaultBounds (..)
  , applyDefaultBoundsToBuildInfo
  , applyDefaultBoundsToDependencies
  , applyDefaultBoundsToExeDependencies
  , applyDefaultBoundsToCondTree
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude

import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.CondTree
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.VersionRange

data DefaultBounds = DefaultBounds
  { defaultTargetBuildDepends :: [Dependency]
  , defaultBuildToolDepends :: [ExeDependency]
  }
  deriving (Generic, Read, Show, Eq, Data, Typeable)

instance Binary DefaultBounds
instance Structured DefaultBounds
instance NFData DefaultBounds where rnf = genericRnf

applyDefaultBoundsToCondTree
  :: L.HasBuildInfo a
  => DefaultBounds
  -> CondTree cv [Dependency] a
  -> CondTree cv [Dependency] a
applyDefaultBoundsToCondTree db =
  mapTreeData (applyDefaultBoundsToBuildInfo db) . mapTreeConstrs (applyDefaultBoundsToDependencies db)

applyDefaultBoundsToBuildInfo :: L.HasBuildInfo a => DefaultBounds -> a -> a
applyDefaultBoundsToBuildInfo db bi =
  bi
    & L.targetBuildDepends %~ applyDefaultBoundsToDependencies db
    & L.buildToolDepends %~ applyDefaultBoundsToExeDependencies db

applyDefaultBoundsToDependencies :: DefaultBounds -> [Dependency] -> [Dependency]
applyDefaultBoundsToDependencies (DefaultBounds bd _) =
  map
    ( \dep@(Dependency pkg vorig l) ->
        if isAnyVersion vorig
          then maybe dep (\v -> Dependency pkg (depVerRange v) l) $ find ((pkg ==) . depPkgName) bd
          else dep
    )

applyDefaultBoundsToExeDependencies :: DefaultBounds -> [ExeDependency] -> [ExeDependency]
applyDefaultBoundsToExeDependencies (DefaultBounds _ btd) =
  map
    ( \dep@(ExeDependency pkg comp vorig) ->
        if isAnyVersion vorig
          then maybe dep (ExeDependency pkg comp . exeDepVerRange) $ find (\(ExeDependency pkg' comp' _) -> pkg == pkg' && comp == comp') btd
          else dep
    )
