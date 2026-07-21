{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Distribution.Client.ProjectPlanning.Stage
--
-- The build 'Stage' tag used as (part of) the elaborated install plan's node
-- key, so that the build-stage and host-stage builds of the same unit are
-- distinct nodes in the plan graph.
module Distribution.Client.ProjectPlanning.Stage
  ( WithStage (..)
  , Stage (..)
  , HasStage (..)
  , Staged (..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Types.ConfiguredId (HasConfiguredId (..))
import Distribution.Compat.Graph (IsNode (..))
import Distribution.Package (HasUnitId (..), Package (..))
import Distribution.Solver.Types.Stage (Stage (..), Staged (..), showStage)
import qualified Text.PrettyPrint as Disp

-- | A value tagged with the build 'Stage' it belongs to.
--
-- Used to wrap the install plan's node key so that a package solved for the
-- build stage and the same package solved for the host stage are kept as
-- distinct graph nodes.
data WithStage a = WithStage Stage a
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Binary a => Binary (WithStage a)
instance Structured a => Structured (WithStage a)

instance Package pkg => Package (WithStage pkg) where
  packageId (WithStage _stage pkg) = packageId pkg

instance IsNode a => IsNode (WithStage a) where
  type Key (WithStage a) = WithStage (Key a)
  nodeKey = fmap nodeKey
  nodeNeighbors = traverse nodeNeighbors

instance HasUnitId a => HasUnitId (WithStage a) where
  installedUnitId (WithStage _stage pkg) = installedUnitId pkg

instance HasConfiguredId a => HasConfiguredId (WithStage a) where
  configuredId (WithStage _stage pkg) = configuredId pkg

-- | The 'Host' stage is the invisible default, so it is not rendered; this
-- keeps the output of non-cross builds unchanged.
instance Pretty a => Pretty (WithStage a) where
  pretty (WithStage Host a) = pretty a
  pretty (WithStage Build a) = Disp.text (showStage Build) <<>> Disp.colon <<>> pretty a

-- | Things that know which build 'Stage' they belong to.
class HasStage a where
  stageOf :: a -> Stage

instance HasStage (WithStage a) where
  stageOf (WithStage s _) = s
