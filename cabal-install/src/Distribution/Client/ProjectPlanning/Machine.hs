{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Distribution.Client.ProjectPlanning.Machine
--
-- The 'Machine' tag used as (part of) the elaborated install plan's node
-- key, so that the build-machine and host-machine builds of the same unit are
-- distinct nodes in the plan graph.
module Distribution.Client.ProjectPlanning.Machine
  ( WithMachine (..)
  , withoutMachine
  , Machine (..)
  , HasMachine (..)
  , PerMachine (..)
  , toolMachine
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Types.ConfiguredId (HasConfiguredId (..))
import Distribution.Compat.Graph (IsNode (..))
import Distribution.Package (HasUnitId (..), Package (..))
import Distribution.Solver.Types.Machine (Machine (..), PerMachine (..), showMachine, toolMachine)
import qualified Text.PrettyPrint as Disp

-- | A value tagged with the 'Machine' it belongs to.
--
-- Used to wrap the install plan's node key so that a package solved for the
-- build machine and the same package solved for the host machine are kept as
-- distinct graph nodes.
data WithMachine a = WithMachine Machine a
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

-- | Drop the machine tag.
withoutMachine :: WithMachine a -> a
withoutMachine (WithMachine _ a) = a

instance Binary a => Binary (WithMachine a)
instance Structured a => Structured (WithMachine a)

instance Package pkg => Package (WithMachine pkg) where
  packageId (WithMachine _machine pkg) = packageId pkg

instance IsNode a => IsNode (WithMachine a) where
  type Key (WithMachine a) = WithMachine (Key a)
  nodeKey = fmap nodeKey
  nodeNeighbors = traverse nodeNeighbors

instance HasUnitId a => HasUnitId (WithMachine a) where
  installedUnitId (WithMachine _machine pkg) = installedUnitId pkg

instance HasConfiguredId a => HasConfiguredId (WithMachine a) where
  configuredId (WithMachine _machine pkg) = configuredId pkg

-- | The 'Host' machine is the invisible default, so it is not rendered; this
-- keeps the output of non-cross builds unchanged.
instance Pretty a => Pretty (WithMachine a) where
  pretty (WithMachine Host a) = pretty a
  pretty (WithMachine Build a) = Disp.text (showMachine Build) <<>> Disp.colon <<>> pretty a

-- | Things that know which 'Machine' they belong to.
class HasMachine a where
  machineOf :: a -> Machine

instance HasMachine (WithMachine a) where
  machineOf (WithMachine s _) = s
