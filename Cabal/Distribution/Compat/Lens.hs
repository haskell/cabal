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
    (?~),
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

infixr 4 .~, %~, ?~

(.~) :: ASetter s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

(?~) :: ASetter s t a (Maybe b) -> b -> s -> t
l ?~ b = set l (Just b)
{-# INLINE (?~) #-}

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
-- First start a repl
--
-- > cabal new-repl Cabal:parser-hackage-tests -fparsec-struct-diff
--
-- Because @--extra-package@ isn't yet implemented, we use a test-suite
-- with @generics-sop@ dependency.
--
-- In the repl, we load a helper script:
--
-- > :l ../generics-sop-lens.hs
--
-- Now we are set up to derive lenses!
--
-- > :m +Distribution.Types.SourceRepo
-- > putStr $ genericLenses (Proxy :: Proxy SourceRepo)
--
-- @
-- repoKind :: Lens' SourceRepo RepoKind
-- repoKind f s = fmap (\\x -> s { T.repoKind = x }) (f (T.repoKind s))
-- \{-# INLINE repoKind #-\}
-- ...
-- @
--
-- /Note:/ You may need to adjust type-aliases, e.g. `String` to `FilePath`.
