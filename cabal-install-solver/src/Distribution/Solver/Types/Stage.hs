{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Distribution.Solver.Types.Stage
  ( Stage (..)
  , showStage
  , prevStage
  , Staged (..)
  , getStage
  , overStage
  , mapWithStage
  , traverseWithStage
  , always
  , isCross
  ) where

import Distribution.Compat.Prelude

-- | The build stage a package is being solved for.
--
-- Cross-compilation distinguishes packages built to /run/ during the build
-- (build-tools, custom @Setup.hs@ dependencies) from packages built to run on
-- the target. Within a 'Distribution.Solver.Types.GoalScope.GoalScope' the
-- stage changes only as the solver crosses tool boundaries (see 'prevStage'),
-- so it follows the qualifier; a namespace, by contrast, is fixed at the root
-- of a goal and inherited unchanged.
data Stage
  = -- | The system where the build is running.
    Build
  | -- | The system where the built artifacts will run.
    Host
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance Binary Stage
instance Structured Stage
instance NFData Stage

-- | The stage name as it appears to users: in constraint scopes (@build:@)
-- and in stage-keyed paths.
showStage :: Stage -> String
showStage Build = "build"
showStage Host = "host"

-- | The stage a tool (a @build-tool-depends@ executable or a custom
-- @Setup.hs@) of a stage-@s@ package is built for. Host packages depend on
-- Build tools when a distinct build toolchain exists; otherwise there is only
-- the host toolchain and tools stay at Host. Build is the earliest stage and
-- clamps.
--
-- This is the only way a stage other than 'Host' comes into existence, so a
-- 'Build' stage is only ever seen when 'isCross' holds for the toolchains.
prevStage :: Staged a -> Stage -> Stage
prevStage tc Host
  | isCross tc = Build
  | otherwise = Host
prevStage _ Build = Build

-- | A value for each build 'Stage' that exists.
--
-- The host stage always exists and always has a value. The build stage
-- exists only under cross-compilation, when there is a build-machine
-- toolchain distinct from the host-machine one; an ordinary build has no
-- build stage at all (@onBuild == Nothing@, constructed with 'always'), not
-- a build stage that happens to equal the host one. Nothing should ask for
-- the build value of such a 'Staged': 'prevStage' never yields 'Build' for
-- it, so no goal, plan node or setup script is ever at 'Build' there.
--
-- Making the build stage a 'Maybe' rather than a second mandatory field means
-- the value itself records whether we are cross-compiling ('isCross'), so no
-- separate flag or stage-set has to be threaded alongside it.
data Staged a = Staged
  { onHost :: a
  , onBuild :: Maybe a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance Binary a => Binary (Staged a)
instance Structured a => Structured (Staged a)
instance NFData a => NFData (Staged a)

-- | Select the value for a given 'Stage'.
--
-- Asking for the build value of a 'Staged' that has no build stage is a
-- bug, not a fallback: a 'Build' stage only ever arises from 'prevStage',
-- which yields it only when 'isCross' holds. The error here is what turns
-- that invariant into something the code enforces.
getStage :: Staged a -> Stage -> a
getStage s Host = onHost s
getStage s Build =
  fromMaybe (error "getStage: no build stage (not cross-compiling)") (onBuild s)

-- | Traverse a 'Staged' with the stage each value belongs to. Only the
-- stages that carry a distinct value are visited, so the build stage of a
-- non-cross build is not (its value is the host's).
traverseWithStage :: Applicative f => (Stage -> a -> f b) -> Staged a -> f (Staged b)
traverseWithStage f (Staged host build) =
  Staged <$> f Host host <*> traverse (f Build) build

-- | Map over a 'Staged' with the stage each value belongs to. Like
-- 'traverseWithStage', the build stage of a non-cross build is not visited.
mapWithStage :: (Stage -> a -> b) -> Staged a -> Staged b
mapWithStage f (Staged host build) = Staged (f Host host) (f Build <$> build)

-- | Apply a function to the value for a given 'Stage', leaving the other
-- stage untouched. Editing the build stage of a non-cross build is a no-op
-- (there is no separate value to edit).
overStage :: Stage -> (a -> a) -> Staged a -> Staged a
overStage Host f s = s{onHost = f (onHost s)}
overStage Build f s = s{onBuild = fmap f (onBuild s)}

-- | The same value for every stage — the non-cross case (no separate build
-- stage).
always :: a -> Staged a
always x = Staged x Nothing

-- | Are we cross-compiling, i.e. is there a distinct build stage?
isCross :: Staged a -> Bool
isCross = isJust . onBuild
