-- | A small prelude used in @zinza@ generated
-- template modules.

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Distribution.ZinzaPrelude (
    Writer,
    execWriter,
    tell,
    -- * Re-exports
    forM_,
    Generic,
    PackageName,
    Version,
    prettyShowBuilder
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad                  (forM_)
import Data.ByteString.Builder        (Builder)
import Distribution.Pretty            (Pretty, prettyShow)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version     (Version)

import qualified Data.ByteString.Builder as BSB

newtype Writer a = W { unW :: Builder -> (Builder, a) }

instance Functor Writer where
    fmap = liftM

instance Applicative Writer where
    pure x = W $ \ss -> (ss, x)
    (<*>) = ap

instance Monad Writer where
    return = pure
    m >>= k = W $ \s1 ->
        let (s2, x) = unW m s1
        in unW (k x) s2
    {-# INLINE (>>=) #-}

execWriter :: Writer a -> Builder
execWriter w = fst (unW w mempty)

tell :: BSB.Builder -> Writer ()
tell s = W $ \s' -> (s' <> s, ())

prettyShowBuilder :: Pretty a => a -> BSB.Builder
prettyShowBuilder = BSB.string8 . prettyShow

-- class ToBuilder a where
--   tell :: a -> Writer ()
--
-- instance {-# OVERLAPPABLE #-} a ~ Builder => ToBuilder a where
--   tell s = W $ \s' -> (s' <> s, ())
--
-- -- instance {-# OVERLAPS #-} ToBuilder String where
-- --   {-# INLINE tell #-}
-- --   tell = tell . BSB.string8
-- --
-- -- instance {-# OVERLAPS #-} ToBuilder Int where
-- --   {-# INLINE tell #-}
-- --   tell = tell . BSB.intDec
