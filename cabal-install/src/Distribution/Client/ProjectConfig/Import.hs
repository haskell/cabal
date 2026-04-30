-- | Project configuration imports.
module Distribution.Client.ProjectConfig.Import
  ( -- * Parsing skeleton
    ProjectConfigSkeleton
  , projectSkeletonImports
  ) where

import Distribution.Client.ProjectConfig.Types
import Distribution.Compat.Lens (view)
import Distribution.PackageDescription (ConfVar (..))
import Distribution.Solver.Types.ProjectConfigPath
import Distribution.Types.CondTree (CondTree (..), traverseCondTreeA)

-- | ProjectConfigSkeleton is a tree of conditional blocks and imports wrapping
-- a config. It can be finalized by providing the conditional resolution info
-- and then resolving and downloading the imports
type ProjectConfigSkeleton = CondTree ConfVar ([ProjectConfigPath], ProjectConfig)

projectSkeletonImports :: ProjectConfigSkeleton -> [ProjectConfigPath]
projectSkeletonImports = fst . view traverseCondTreeA
