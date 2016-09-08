{-# LANGUAGE CPP #-}

#ifdef MIN_VERSION_base
#define MINVER_base_48 MIN_VERSION_base(4,8,0)
#define MINVER_base_47 MIN_VERSION_base(4,7,0)
#define MINVER_base_46 MIN_VERSION_base(4,6,0)
#else
#define MINVER_base_48 (__GLASGOW_HASKELL__ >= 710)
#define MINVER_base_47 (__GLASGOW_HASKELL__ >= 708)
#define MINVER_base_46 (__GLASGOW_HASKELL__ >= 706)
#endif

-- | This module does two things:
--
-- * Acts as a compatiblity layer, like @base-compat@.
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
    Typeable,
    Data,
    Generic,
    NFData (..),
    Binary (..),
    Alternative (..),
    MonadPlus (..),

    -- * Some types
    IO, NoCallStackIO,
    Map,

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

    -- * Data.Foldable
    Foldable, foldMap, foldr,
    null, length,
    find, foldl',
    traverse_, for_,

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

    -- * Text.PrettyPrint
    (<<>>),
    ) where

-- We also could hide few partial function
import Prelude                       as BasePrelude hiding
  ( IO, mapM, mapM_, sequence, null, length, foldr
#if MINVER_base_48
  -- We hide them, as we import only some members
  , Traversable, traverse, sequenceA
  , Foldable, foldMap
#endif
  )

#if !MINVER_base_48
import Control.Applicative           (Applicative (..), (<$), (<$>))
import Distribution.Compat.Semigroup (Monoid (..))
#else
import Data.Foldable                 (length, null)
#endif

import Data.Foldable                 (Foldable (foldMap, foldr), find, foldl', for_, traverse_)
import Data.Traversable              (Traversable (traverse, sequenceA), for)

import Control.Applicative           (Alternative (..))
import Control.DeepSeq               (NFData (..))
import Data.Data                     (Data)
import Data.Typeable                 (Typeable)
import Distribution.Compat.Binary    (Binary (..))
import Distribution.Compat.Semigroup (Semigroup (..), gmappend, gmempty)
import GHC.Generics                  (Generic)

import Data.Map                      (Map)

import Control.Arrow                 (first)
import Control.Monad                 hiding (mapM)
import Data.Char
import Data.List                     (intercalate, intersperse, isPrefixOf,
                                      isSuffixOf, nub, nubBy, sort, sortBy,
                                      unfoldr)
import Data.Maybe

import qualified Text.PrettyPrint as Disp

import qualified Prelude as OrigPrelude
import Distribution.Compat.Stack

type IO a = WithCallStack (OrigPrelude.IO a)
type NoCallStackIO a = OrigPrelude.IO a

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
