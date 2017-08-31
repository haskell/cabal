{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | Per Conor McBride, the 'Newtype' typeclass represents the packing and
-- unpacking of a newtype, and allows you to operatate under that newtype with
-- functions such as 'ala'.
module Distribution.Compat.Newtype (
    Newtype (..),
    ala,
    ala',
    pack',
    unpack',
    ) where

import Data.Functor.Identity (Identity (..))

class Newtype n o | n -> o where
    pack   :: o -> n
    unpack :: n -> o

instance Newtype (Identity a) a where
    pack   = Identity
    unpack = runIdentity

ala :: (Newtype n o, Newtype n' o') => (o -> n) -> ((o -> n) -> b -> n') -> (b -> o')
ala pa hof = ala' pa hof id

ala' :: (Newtype n o, Newtype n' o') => (o -> n) -> ((a -> n) -> b -> n') -> (a -> o) -> (b -> o')
ala' _ hof f = unpack . hof (pack . f)

-- | Variant of 'pack', which takes a phantom type.
pack' :: Newtype n o => (o -> n) -> o -> n
pack' _ = pack

-- | Variant of 'pack', which takes a phantom type.
unpack' :: Newtype n o => (o -> n) -> n -> o
unpack' _ = unpack
