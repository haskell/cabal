{-# LANGUAGE RankNTypes #-}
-- | This module provides very basic lens functionality, without extra dependencies.
--
-- For the documentation of the combinators see <http://hackage.haskell.org/package/lens lens> package.
-- This module uses the same vocabulary.
module Distribution.Compat.Lens (
    -- * Types
    Lens,
    Lens',
    Traversal,
    Traversal',
    -- ** rank-1 types
    Getting,
    ASetter,
    -- * Getter
    view,
    -- * Setter
    set,
    over,
    -- * Fold
    toDListOf,
    toListOf,
    toSetOf,
    -- * Common lenses
    _1, _2,
    -- * Operators
    (&),
    (.~), (%~),
    -- * Cabal developer info
    -- $development
    ) where

import Prelude()
import Distribution.Compat.Prelude

import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (..))

import qualified Distribution.Compat.DList as DList
import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type Lens      s t a b = forall f. Functor f     => (a -> f b) -> s -> f t
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type Lens'      s a = forall f. Functor f     => (a -> f a) -> s -> f s
type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

type Getting r s a = (a -> Const r a) -> s -> Const r s
type ASetter s t a b = (a -> Identity b) -> s -> Identity t

-------------------------------------------------------------------------------
-- Getter
-------------------------------------------------------------------------------

view :: s -> Getting a s a -> a
view s l = getConst (l Const s)

-------------------------------------------------------------------------------
-- Setter
-------------------------------------------------------------------------------

set :: ASetter s t a  b -> b -> s -> t
set l x = over l (const x)

over :: ASetter s t a b -> (a -> b) -> s -> t
over l f s = runIdentity (l (\x -> Identity (f x)) s)

-------------------------------------------------------------------------------
-- Fold
-------------------------------------------------------------------------------

toDListOf :: Getting (DList.DList a) s a -> s -> DList.DList a
toDListOf l s = getConst (l (\x -> Const (DList.singleton x)) s)

toListOf :: Getting (DList.DList a) s a -> s -> [a]
toListOf l = DList.runDList . toDListOf l

toSetOf  :: Getting (Set.Set a) s a -> s -> Set.Set a
toSetOf l s = getConst (l (\x -> Const (Set.singleton x)) s)

-------------------------------------------------------------------------------
-- Lens
-------------------------------------------------------------------------------

{-
lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens sa sbt afb s = sbt s <$> afb (sa s)
-}

-------------------------------------------------------------------------------
-- Common
-------------------------------------------------------------------------------

_1 ::  Lens (a, c) (b, c) a b
_1 f (a, c) = flip (,) c <$> f a

_2 ::  Lens (c, a) (c, b) a b
_2 f (c, a) = (,) c <$> f a

-------------------------------------------------------------------------------
-- Operators
-------------------------------------------------------------------------------


-- | '&' is a reverse application operator
(&) :: a -> (a -> b) -> b
(&) = flip ($)
{-# INLINE (&) #-}
infixl 1 &

infixr 4 .~, %~

(.~) :: ASetter s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

-------------------------------------------------------------------------------
-- Documentation
-------------------------------------------------------------------------------

-- $development
--
-- We cannot depend on @template-haskell@, because Cabal is a boot library.
-- This fact makes defining optics a manual task. Here is a small recipe to
-- make the process less tedious.
--
-- First start a repl with proper-lens dependency
--
-- > cabal new-repl Cabal:lib:Cabal ???
--
-- or
--
-- > stack ghci Cabal:lib --package lens
--
-- Then enable Template Haskell and the dumping of splices:
--
-- > :set -XTemplateHaskell -ddump-slices
--
-- Now we can derive lenses, load appropriate modules:
--
-- > :m Control.Lens Distribution.PackageDescription
--
-- and let Template Haskell do the job:
--
-- >  ; makeLensesWith (lensRules & lensField .~ mappingNamer return) ''GenericPackageDescription
--
-- At this point, we will get unfancy splices looking like
--
-- @
-- condBenchmarks ::
--   'Lens'' GenericPackageDescription [(UnqualComponentName,
--                                     CondTree ConfVar [Dependency] Benchmark)]
-- condBenchmarks
--   f_a2UEg
--   (GenericPackageDescription x1_a2UEh
--                              x2_a2UEi
--                              x3_a2UEj
--                              x4_a2UEk
--                              x5_a2UEl
--                              x6_a2UEm
--                              x7_a2UEn
--                              x8_a2UEo)
--   = fmap
--       (\\ y1_a2UEp
--          -> GenericPackageDescription
--               x1_a2UEh
--               x2_a2UEi
--               x3_a2UEj
--               x4_a2UEk
--               x5_a2UEl
--               x6_a2UEm
--               x7_a2UEn
--               y1_a2UEp)
--       (f_a2UEg x8_a2UEo)
-- {-\# INLINE condBenchmarks \#-}
-- @
--
-- yet they can be cleaned up with e.g. VIM magic regexp and @hindent@:
--
-- > :%s/\v(\w+)_\w+/\1/g
-- > :%!hindent --indent-size 4 --line-length 200
--
-- Resulting into
--
-- @
-- condBenchmarks :: 'Lens'' GenericPackageDescription [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)]
-- condBenchmarks f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) =
--     fmap (\\y1 -> GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 y1) (f x8)
-- {-\# INLINE condBenchmarks \#-}
-- @
