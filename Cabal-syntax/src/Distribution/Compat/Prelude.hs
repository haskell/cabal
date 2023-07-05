{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

{- FOURMOLU_DISABLE -}
#ifdef MIN_VERSION_base
#define MINVER_base_411 MIN_VERSION_base(4,11,0)
#else
#define MINVER_base_411 (__GLASGOW_HASKELL__ >= 804)
#endif
{- FOURMOLU_ENABLE -}

-- | This module does two things:
--
-- * Acts as a compatibility layer, like @base-compat@.
--
-- * Provides commonly used imports.
module Distribution.Compat.Prelude
  ( -- * Prelude

  --
  -- Prelude is re-exported, following is hidden:
      module BasePrelude

    -- * Common type-classes
  , Semigroup (..)
  , gmappend
  , gmempty
  , Typeable
  , TypeRep
  , typeRep
  , Data
  , Generic
  , NFData (..)
  , genericRnf
  , Binary (..)
  , Structured
  , Alternative (..)
  , MonadPlus (..)
  , IsString (..)

    -- * Some types
  , Map
  , Set
  , NonEmptySet
  , Identity (..)
  , Proxy (..)
  , Const (..)
  , Void

    -- * Data.Either
  , partitionEithers

    -- * Data.Maybe
  , catMaybes
  , mapMaybe
  , fromMaybe
  , maybeToList
  , listToMaybe
  , isNothing
  , isJust

    -- * Data.List
  , unfoldr
  , isPrefixOf
  , isSuffixOf
  , intercalate
  , intersperse
  , sort
  , sortBy
  , nub
  , nubBy
  , partition
  , dropWhileEnd

    -- * Data.List.NonEmpty
  , NonEmpty ((:|))
  , nonEmpty
  , foldl1
  , foldr1
  , head
  , tail
  , last
  , init

    -- * Data.Foldable
  , Foldable
  , foldMap
  , foldr
  , null
  , length
  , find
  , foldl'
  , traverse_
  , for_
  , any
  , all
  , toList

    -- * Data.Traversable
  , Traversable
  , traverse
  , sequenceA
  , for

    -- * Data.Function
  , on

    -- * Data.Ord
  , comparing

    -- * Control.Arrow
  , first

    -- * Control.Monad
  , liftM
  , liftM2
  , unless
  , when
  , ap
  , void
  , foldM
  , filterM
  , join
  , guard

    -- * Control.Exception
  , catch
  , throwIO
  , evaluate
  , Exception (..)
  , IOException
  , SomeException (..)
  , tryIO
  , catchIO
  , catchExit

    -- * Control.DeepSeq
  , deepseq
  , force

    -- * Data.Char
  , isSpace
  , isDigit
  , isUpper
  , isAlpha
  , isAlphaNum
  , chr
  , ord
  , toLower
  , toUpper

    -- * Data.Void
  , absurd
  , vacuous

    -- * Data.Word & Data.Int
  , Word
  , Word8
  , Word16
  , Word32
  , Word64
  , Int8
  , Int16
  , Int32
  , Int64

    -- * Text.PrettyPrint
  , (<<>>)
  , (Disp.<+>)

    -- * System.Exit
  , ExitCode (..)
  , exitWith
  , exitSuccess
  , exitFailure

    -- * Text.Read
  , readMaybe

    -- * Debug.Trace (as deprecated functions)
  , trace
  , traceShow
  , traceShowId
  , traceM
  , traceShowM
  ) where

-- We also could hide few partial function
{- FOURMOLU_DISABLE -}
import Prelude                       as BasePrelude hiding
    ( mapM, mapM_, sequence, any, all, head, tail, last, init
    -- partial functions
    , read
#if MINVER_base_411
    -- As of base 4.11.0.0 Prelude exports part of Semigroup(..).
    -- Hide this so we instead rely on Distribution.Compat.Semigroup.
    , Semigroup(..)
#endif
    , Word
    -- We hide them, as we import only some members
    , Traversable, traverse, sequenceA
    , Foldable(..)
    )
{- FOURMOLU_ENABLE -}
import Data.Foldable as BasePrelude (elem, foldl, maximum, minimum, product, sum)

-- AMP
import Data.Foldable
  ( Foldable (foldMap, foldr, toList)
  , all
  , any
  , find
  , foldl'
  , for_
  , length
  , null
  , traverse_
  )
import Data.Traversable (Traversable (sequenceA, traverse), for)

import qualified Data.Foldable

-- Extra exports
import Control.Applicative (Alternative (..), Const (..))
import Control.Arrow (first)
import Control.DeepSeq (NFData (..), deepseq, force)
import Control.Exception (Exception (..), IOException, SomeException (..), catch, evaluate, throwIO)
import Control.Monad (MonadPlus (..), ap, filterM, foldM, guard, join, liftM, liftM2, unless, void, when)
import Data.Char (chr, isAlpha, isAlphaNum, isDigit, isSpace, isUpper, ord, toLower, toUpper)
import Data.Data (Data)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (dropWhileEnd, intercalate, intersperse, isPrefixOf, isSuffixOf, nub, nubBy, partition, sort, sortBy, unfoldr)
import Data.List.NonEmpty (NonEmpty ((:|)), head, init, last, nonEmpty, tail)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.String (IsString (..))
import Data.Void (Void, absurd, vacuous)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Distribution.Compat.Binary (Binary (..))
import Distribution.Compat.Semigroup (Semigroup (..), gmappend, gmempty)
import Distribution.Compat.Typeable (TypeRep, Typeable, typeRep)
import GHC.Generics (Generic (..), K1 (unK1), M1 (unM1), U1 (U1), V1, (:*:) ((:*:)), (:+:) (L1, R1))
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import Text.Read (readMaybe)

import qualified Text.PrettyPrint as Disp

import Distribution.Compat.Exception
import Distribution.Compat.NonEmptySet (NonEmptySet)
import Distribution.Utils.Structured (Structured)

import qualified Debug.Trace

-- | New name for 'Text.PrettyPrint.<>'
(<<>>) :: Disp.Doc -> Disp.Doc -> Disp.Doc
(<<>>) = (Disp.<>)

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

-------------------------------------------------------------------------------
-- Trace
-------------------------------------------------------------------------------

-- Functions from Debug.Trace
-- but with DEPRECATED pragma, so -Werror will scream on them.

trace :: String -> a -> a
trace = Debug.Trace.trace
{-# DEPRECATED trace "Don't leave me in the code" #-}

traceShowId :: Show a => a -> a
traceShowId x = Debug.Trace.traceShow x x
{-# DEPRECATED traceShowId "Don't leave me in the code" #-}

traceShow :: Show a => a -> b -> b
traceShow = Debug.Trace.traceShow
{-# DEPRECATED traceShow "Don't leave me in the code" #-}

traceM :: Applicative f => String -> f ()
traceM = Debug.Trace.traceM
{-# DEPRECATED traceM "Don't leave me in the code" #-}

traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Debug.Trace.traceShowM
{-# DEPRECATED traceShowM "Don't leave me in the code" #-}
