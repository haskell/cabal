-- | A small prelude used in @zinza@ generated
-- template modules.
module Distribution.ZinzaPrelude (
    Writer,
    execWriter,
    tell,
    -- * Re-exports
    forM_,
    Generic,
    PackageName,
    Version,
    prettyShow
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad                  (forM_)
import Data.ByteString.Builder        (Builder)
import Distribution.Pretty            (prettyShow)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version     (Version)

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

tell :: Builder -> Writer ()
tell s = W $ \s' -> (s' <> s, ())
