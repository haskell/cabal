{-# LANGUAGE CPP #-}

module Distribution.Utils.MapAccum (mapAccumM) where

import Distribution.Compat.Prelude
import Prelude ()

-- Like StateT but with return tuple swapped
newtype StateM s m a = StateM {runStateM :: s -> m (s, a)}

instance Functor m => Functor (StateM s m) where
  fmap f (StateM x) = StateM $ \s -> fmap (\(s', a) -> (s', f a)) (x s)

instance Monad m => Applicative (StateM s m) where
  pure x = StateM $ \s -> return (s, x)
  StateM f <*> StateM x = StateM $ \s -> do
    (s', f') <- f s
    (s'', x') <- x s'
    return (s'', f' x')

-- | Monadic variant of 'mapAccumL'.
mapAccumM
  :: (Monad m, Traversable t)
  => (a -> b -> m (a, c))
  -> a
  -> t b
  -> m (a, t c)
mapAccumM f s t = runStateM (traverse (\x -> StateM (\s' -> f s' x)) t) s
