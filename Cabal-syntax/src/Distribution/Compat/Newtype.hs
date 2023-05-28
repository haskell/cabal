{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Per Conor McBride, the 'Newtype' typeclass represents the packing and
-- unpacking of a newtype, and allows you to operate under that newtype with
-- functions such as 'ala'.
module Distribution.Compat.Newtype
  ( Newtype (..)
  , ala
  , alaf
  , pack'
  , unpack'
  ) where

import Data.Functor.Identity (Identity (..))
import Data.Monoid (Endo (..), Product (..), Sum (..))

#if MIN_VERSION_base(4,7,0)
import Data.Coerce (coerce, Coercible)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif

-- | The @FunctionalDependencies@ version of 'Newtype' type-class.
--
-- Since Cabal-3.0 class arguments are in a different order than in @newtype@ package.
-- This change is to allow usage with @DeriveAnyClass@ (and @DerivingStrategies@, in GHC-8.2).
-- Unfortunately one has to repeat inner type.
--
-- @
-- newtype New = New Old
--   deriving anyclass (Newtype Old)
-- @
--
-- Another approach would be to use @TypeFamilies@ (and possibly
-- compute inner type using "GHC.Generics"), but we think @FunctionalDependencies@
-- version gives cleaner type signatures.
{- FOURMOLU_DISABLE -}
class Newtype o n | n -> o where
  pack :: o -> n
#if MIN_VERSION_base(4,7,0)
  default pack :: Coercible o n => o -> n
  pack = coerce
#else
  default pack :: o -> n
  pack = unsafeCoerce
#endif

  unpack :: n -> o
#if MIN_VERSION_base(4,7,0)
  default unpack :: Coercible n o => n -> o
  unpack = coerce
#else
  default unpack :: n -> o
  unpack = unsafeCoerce
#endif
{- FOURMOLU_ENABLE -}

instance Newtype a (Identity a)
instance Newtype a (Sum a)
instance Newtype a (Product a)
instance Newtype (a -> a) (Endo a)

-- |
--
-- >>> ala Sum foldMap [1, 2, 3, 4 :: Int]
-- 10
--
-- /Note:/ the user supplied function for the newtype is /ignored/.
--
-- >>> ala (Sum . (+1)) foldMap [1, 2, 3, 4 :: Int]
-- 10
ala :: (Newtype o n, Newtype o' n') => (o -> n) -> ((o -> n) -> b -> n') -> (b -> o')
ala pa hof = alaf pa hof id

-- |
--
-- >>> alaf Sum foldMap length ["cabal", "install"]
-- 12
--
-- /Note:/ as with 'ala', the user supplied function for the newtype is /ignored/.
alaf :: (Newtype o n, Newtype o' n') => (o -> n) -> ((a -> n) -> b -> n') -> (a -> o) -> (b -> o')
alaf _ hof f = unpack . hof (pack . f)

-- | Variant of 'pack', which takes a phantom type.
pack' :: Newtype o n => (o -> n) -> o -> n
pack' _ = pack

-- | Variant of 'unpack', which takes a phantom type.
unpack' :: Newtype o n => (o -> n) -> n -> o
unpack' _ = unpack
