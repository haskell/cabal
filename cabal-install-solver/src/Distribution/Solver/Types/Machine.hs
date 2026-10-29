{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Distribution.Solver.Types.Machine
  ( Machine (..)
  , showMachine
  , toolMachine
  , PerMachine (..)
  , onMachine
  , overMachine
  , mapWithMachine
  , traverseWithMachine
  , always
  , isCross
  ) where

import Distribution.Compat.Prelude

-- | The machine whose toolchain a package is compiled with.
--
-- The names are autoconf's: the /build/ machine is where the build runs, the
-- /host/ machine is where the built artifacts will run. In an ordinary build
-- they are the same machine; cross-compilation distinguishes packages built
-- to /run/ during the build (build-tools, custom @Setup.hs@ dependencies),
-- which are compiled with the build machine's toolchain, from everything
-- else, which is compiled with the host machine's. A 'Machine' is not a phase
-- of the build: nothing is ordered by it, it only selects a toolchain (see
-- 'PerMachine').
--
-- Within a 'Distribution.Solver.Types.GoalScope.GoalScope' the machine
-- changes only as the solver crosses tool boundaries (see 'toolMachine'), so
-- it follows the qualifier; a namespace, by contrast, is fixed at the root of
-- a goal and inherited unchanged.
data Machine
  = -- | The machine the build runs on.
    Build
  | -- | The machine the built artifacts will run on.
    Host
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance Binary Machine
instance Structured Machine
instance NFData Machine

-- | The machine name as it appears to users: in constraint scopes (@build:@)
-- and in machine-keyed paths.
showMachine :: Machine -> String
showMachine Build = "build"
showMachine Host = "host"

-- | The machine a tool (a @build-tool-depends@ executable or a custom
-- @Setup.hs@) of a package on machine @m@ is built for: tools have to run
-- where the build runs. Host packages depend on Build tools when a distinct
-- build toolchain exists; otherwise there is only the host toolchain and
-- tools stay at Host. Tools of Build packages are Build too.
--
-- This is the only way a machine other than 'Host' comes into existence, so a
-- 'Build' machine is only ever seen when 'isCross' holds for the toolchains.
toolMachine :: PerMachine a -> Machine -> Machine
toolMachine tc Host
  | isCross tc = Build
  | otherwise = Host
toolMachine _ Build = Build

-- | A value for each 'Machine' that has its own toolchain.
--
-- The host machine always exists and always has a value. The build machine
-- exists only under cross-compilation, when there is a build-machine
-- toolchain distinct from the host-machine one; an ordinary build has no
-- build machine at all (@onBuild == Nothing@, constructed with 'always'), not
-- a build machine that happens to equal the host one. Nothing should ask for
-- the build value of such a 'PerMachine': 'toolMachine' never yields 'Build' for
-- it, so no goal, plan node or setup script is ever at 'Build' there.
--
-- Making the build machine a 'Maybe' rather than a second mandatory field means
-- the value itself records whether we are cross-compiling ('isCross'), so no
-- separate flag or machine-set has to be threaded alongside it.
data PerMachine a = PerMachine
  { onHost :: a
  , onBuild :: Maybe a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance Binary a => Binary (PerMachine a)
instance Structured a => Structured (PerMachine a)
instance NFData a => NFData (PerMachine a)

-- | Select the value for a given 'Machine'.
--
-- Asking for the build value of a 'PerMachine' that has no build machine is a
-- bug, not a fallback: a 'Build' machine only ever arises from 'toolMachine',
-- which yields it only when 'isCross' holds. The error here is what turns
-- that invariant into something the code enforces.
onMachine :: PerMachine a -> Machine -> a
onMachine s Host = onHost s
onMachine s Build =
  fromMaybe (error "onMachine: no build machine (not cross-compiling)") (onBuild s)

-- | Traverse a 'PerMachine' with the machine each value belongs to. Only the
-- machines that carry a distinct value are visited, so the build machine of a
-- non-cross build is not (its value is the host's).
traverseWithMachine :: Applicative f => (Machine -> a -> f b) -> PerMachine a -> f (PerMachine b)
traverseWithMachine f (PerMachine host build) =
  PerMachine <$> f Host host <*> traverse (f Build) build

-- | Map over a 'PerMachine' with the machine each value belongs to. Like
-- 'traverseWithMachine', the build machine of a non-cross build is not visited.
mapWithMachine :: (Machine -> a -> b) -> PerMachine a -> PerMachine b
mapWithMachine f (PerMachine host build) = PerMachine (f Host host) (f Build <$> build)

-- | Apply a function to the value for a given 'Machine', leaving the other
-- machine untouched. Editing the build machine of a non-cross build is a no-op
-- (there is no separate value to edit).
overMachine :: Machine -> (a -> a) -> PerMachine a -> PerMachine a
overMachine Host f s = s{onHost = f (onHost s)}
overMachine Build f s = s{onBuild = fmap f (onBuild s)}

-- | The same value for every machine — the non-cross case (no separate build
-- machine).
always :: a -> PerMachine a
always x = PerMachine x Nothing

-- | Are we cross-compiling, i.e. is there a distinct build machine?
isCross :: PerMachine a -> Bool
isCross = isJust . onBuild
