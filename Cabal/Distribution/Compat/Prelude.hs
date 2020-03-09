{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}

#ifdef MIN_VERSION_base
#define MINVER_base_411 MIN_VERSION_base(4,11,0)
#define MINVER_base_48 MIN_VERSION_base(4,8,0)
#define MINVER_base_47 MIN_VERSION_base(4,7,0)
#else
#define MINVER_base_411 (__GLASGOW_HASKELL__ >= 804)
#define MINVER_base_48 (__GLASGOW_HASKELL__ >= 710)
#define MINVER_base_47 (__GLASGOW_HASKELL__ >= 708)
#endif

-- | This module does two things:
--
-- * Acts as a compatibility layer, like @base-compat@.
--
-- * Provides commonly used imports.
module Distribution.Compat.Prelude (
    -- * Prelude
    --
    -- Prelude is re-exported, following is hidden:
    module BasePrelude,

#if !MINVER_base_48
    -- * base 4.8 shim
    Applicative(..), (<$), (<$>),
    Monoid(..),
#endif

    -- * Common type-classes
    Semigroup (..),
    gmappend, gmempty,
    Typeable, TypeRep, typeRep,
    Data,
    Generic,
    NFData (..), genericRnf,
    Binary (..),
    Structured,
    Alternative (..),
    MonadPlus (..),
    IsString (..),

    -- * Some types
    Map,
    Set,
    Identity (..),
    Proxy (..),
    Void,

    -- * Data.Maybe
    catMaybes, mapMaybe,
    fromMaybe,
    maybeToList, listToMaybe,
    isNothing, isJust,

    -- * Data.List
    unfoldr,
    isPrefixOf, isSuffixOf,
    intercalate, intersperse,
    sort, sortBy,
    nub, nubBy,

    -- * Data.List.NonEmpty
    NonEmpty((:|)), foldl1, foldr1,
    head, tail, last, init,

    -- * Data.Foldable
    Foldable, foldMap, foldr,
    null, length,
    find, foldl',
    traverse_, for_,
    any, all,
    toList,

    -- * Data.Traversable
    Traversable, traverse, sequenceA,
    for,

    -- * Control.Arrow
    first,

    -- * Control.Monad
    liftM, liftM2,
    unless, when,
    ap, void,
    foldM, filterM,

    -- * Data.Char
    isSpace, isDigit, isUpper, isAlpha, isAlphaNum,
    chr, ord,
    toLower, toUpper,

    -- * Data.Void
    absurd, vacuous,

    -- * Data.Word & Data.Int
    Word,
    Word8, Word16, Word32, Word64,
    Int8, Int16, Int32, Int64,

    -- * Text.PrettyPrint
    (<<>>),

    -- * Text.Read
    readMaybe,
    ) where
-- We also could hide few partial function
import Prelude                       as BasePrelude hiding
  ( mapM, mapM_, sequence, null, length, foldr, any, all, head, tail, last, init
  -- partial functions
  , read
  , foldr1, foldl1
#if MINVER_base_411
  -- As of base 4.11.0.0 Prelude exports part of Semigroup(..).
  -- Hide this so we instead rely on Distribution.Compat.Semigroup.
  , Semigroup(..)
#endif
#if MINVER_base_48
  , Word
  -- We hide them, as we import only some members
  , Traversable, traverse, sequenceA
  , Foldable, foldMap
#endif
  )

#if !MINVER_base_48
import Control.Applicative           (Applicative (..), (<$), (<$>))
import Distribution.Compat.Semigroup (Monoid (..))
import Data.Foldable                 (toList)
#else
import Data.Foldable                 (length, null, Foldable(toList))
#endif

import Data.Foldable                 (Foldable (foldMap, foldr), find, foldl', for_, traverse_, any, all)
import Data.Traversable              (Traversable (traverse, sequenceA), for)
import qualified Data.Foldable

import Control.Applicative           (Alternative (..))
import Control.DeepSeq               (NFData (..))
import Data.Data                     (Data)
import Distribution.Compat.Typeable  (Typeable, TypeRep, typeRep)
import Distribution.Compat.Binary    (Binary (..))
import Distribution.Compat.Semigroup (Semigroup (..), gmappend, gmempty)
import GHC.Generics                  (Generic, Rep(..),
                                      V1, U1(U1), K1(unK1), M1(unM1),
                                      (:*:)((:*:)), (:+:)(L1,R1))

import Data.Functor.Identity         (Identity (..))
import Data.Map                      (Map)
import Data.Proxy                    (Proxy (..))
import Data.Set                      (Set)

import Control.Arrow                 (first)
import Control.Monad                 hiding (mapM)
import Data.Char
import Data.List                     (intercalate, intersperse, isPrefixOf,
                                      isSuffixOf, nub, nubBy, sort, sortBy,
                                      unfoldr)
import Data.List.NonEmpty            (NonEmpty((:|)), head, tail, init, last)
import Data.Maybe
import Data.String                   (IsString (..))
import Data.Int
import Data.Word
import Data.Void                     (Void, absurd, vacuous)
import Text.Read                     (readMaybe)

import qualified Text.PrettyPrint as Disp

import Distribution.Utils.Structured (Structured)

-- | New name for 'Text.PrettyPrint.<>'
(<<>>) :: Disp.Doc -> Disp.Doc -> Disp.Doc
(<<>>) = (Disp.<>)

#if !MINVER_base_48
-- | Test whether the structure is empty. The default implementation is
-- optimized for structures that are similar to cons-lists, because there
-- is no general way to do better.
null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True

-- | Returns the size/length of a finite structure as an 'Int'.  The
-- default implementation is optimized for structures that are similar to
-- cons-lists, because there is no general way to do better.
length :: Foldable t => t a -> Int
length = foldl' (\c _ -> c+1) 0
#endif


-- | "GHC.Generics"-based 'rnf' implementation
--
-- This is needed in order to support @deepseq < 1.4@ which didn't
-- have a 'Generic'-based default 'rnf' implementation yet.
--
-- In order to define instances, use e.g.
--
-- > instance NFData MyType where rnf = genericRnf
--
-- The implementation has been taken from @deepseq-1.4.2@'s default
-- 'rnf' implementation.
genericRnf :: (Generic a, GNFData (Rep a)) => a -> ()
genericRnf = grnf . from

-- | Hidden internal type-class
class GNFData f where
  grnf :: f a -> ()

instance GNFData V1 where
  grnf = error "Control.DeepSeq.rnf: uninhabited type"

instance GNFData U1 where
  grnf U1 = ()

instance NFData a => GNFData (K1 i a) where
  grnf = rnf . unK1
  {-# INLINEABLE grnf #-}

instance GNFData a => GNFData (M1 i c a) where
  grnf = grnf . unM1
  {-# INLINEABLE grnf #-}

instance (GNFData a, GNFData b) => GNFData (a :*: b) where
  grnf (x :*: y) = grnf x `seq` grnf y
  {-# INLINEABLE grnf #-}

instance (GNFData a, GNFData b) => GNFData (a :+: b) where
  grnf (L1 x) = grnf x
  grnf (R1 x) = grnf x
  {-# INLINEABLE grnf #-}


-- TODO: if we want foldr1/foldl1 to work on more than NonEmpty, we
-- can define a local typeclass 'Foldable1', e.g.
--
-- @
-- class Foldable f => Foldable1 f
--
-- instance Foldable1 NonEmpty
--
-- foldr1 :: Foldable1 t => (a -> a -> a) -> t a -> a
-- foldr1 = Data.Foldable.foldr1
--
-- foldl1 :: Foldable1 t => (a -> a -> a) -> t a -> a
-- foldl1 = Data.Foldable.foldl1
-- @
--

{-# INLINE foldr1 #-}
foldr1 :: (a -> a -> a) -> NonEmpty a -> a
foldr1 = Data.Foldable.foldr1

{-# INLINE foldl1 #-}
foldl1 :: (a -> a -> a) -> NonEmpty a -> a
foldl1 = Data.Foldable.foldl1
