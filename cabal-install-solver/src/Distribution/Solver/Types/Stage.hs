{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Distribution.Solver.Types.Stage
  ( Stage (..)
  , showStage
  , prevStage
  , Staged (..)
  , getStage
  , overStage
  , always
  , isCross
  , activeStages
  ) where

import Distribution.Compat.Prelude

-- | The build stage a package is being solved for.
--
-- Cross-compilation distinguishes packages built to /run/ during the build
-- (build-tools, custom @Setup.hs@ dependencies) from packages built to run on
-- the target. This is an axis orthogonal to 'Distribution.Solver.Types.PackagePath.Namespace':
-- the stage changes as the solver crosses tool boundaries (see 'prevStage'),
-- whereas a namespace is fixed at the root of a goal and inherited unchanged.
data Stage
  = -- | The system where the build is running.
    Build
  | -- | The system where the built artifacts will run.
    Host
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance Binary Stage
instance Structured Stage

-- | The stage name as it appears to users: in constraint scopes (@build:@)
-- and in stage-keyed paths.
showStage :: Stage -> String
showStage Build = "build"
showStage Host = "host"

-- | The stage a tool dependency (build-tool or setup) is solved for, relative
-- to the depending package's stage. Host packages depend on Build tools;
-- Build is the earliest stage and clamps.
prevStage :: Stage -> Stage
prevStage s
  | s == minBound = s
  | otherwise = pred s

-- | A value provided for each build 'Stage'.
--
-- The host stage always has a value. The build stage is optional: an ordinary
-- (non-cross) build has no separate build stage (@onBuild == Nothing@,
-- constructed with 'always'), so it reuses the host value; under cross-
-- compilation the build stage carries its own value (e.g. a build-machine
-- compiler distinct from the host-machine one).
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

-- | Select the value for a given 'Stage'. The build stage falls back to the
-- host value when there is no separate build stage (a non-cross build), so
-- every stage is always resolvable.
getStage :: Staged a -> Stage -> a
getStage s Host = onHost s
getStage s Build = fromMaybe (onHost s) (onBuild s)

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

-- | The stages that carry a distinct value: always 'Host', plus 'Build' when
-- cross-compiling. This is the set of stages worth solving\/converting
-- separately.
activeStages :: Staged a -> [Stage]
activeStages s = Host : [Build | isCross s]
