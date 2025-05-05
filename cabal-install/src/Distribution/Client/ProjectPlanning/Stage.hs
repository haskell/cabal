{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

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
import Distribution.Solver.Types.Stage (Stage (..), Staged (..))
import Text.PrettyPrint (colon)

-- FIXME: blaaah
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

instance Pretty a => Pretty (WithStage a) where
  pretty (WithStage s pkg) = pretty s <> colon <> pretty pkg

class HasStage a where
  stageOf :: a -> Stage

instance HasStage (WithStage a) where
  stageOf (WithStage s _) = s
