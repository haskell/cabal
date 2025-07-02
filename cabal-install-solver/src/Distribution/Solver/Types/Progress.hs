{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module Distribution.Solver.Types.Progress
    ( Progress(..)
    , foldProgress
    , step
    , fail
    ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude

-- | A type to represent the unfolding of an expensive long running
-- calculation that may fail. We may get intermediate steps before the final
-- result which may be used to indicate progress and\/or logging messages.
--
data Progress step fail done = Step step (Progress step fail done)
                             | Fail fail
                             | Done done
  deriving (Functor)

step :: step -> Progress step fail ()
step s = Step s (Done ())

-- | Consume a 'Progress' calculation. Much like 'foldr' for lists but with two
-- base cases, one for a final result and one for failure.
--
-- Eg to convert into a simple 'Either' result use:
--
-- > foldProgress (flip const) Left Right
--
foldProgress :: (step -> a -> a) -> (fail -> a) -> (done -> a)
             -> Progress step fail done -> a
foldProgress step_ fail_ done_ = fold
  where fold (Step s p) = step_ s (fold p)
        fold (Fail f)   = fail_ f
        fold (Done r)   = done_ r

instance Monad (Progress step fail) where
  return   = pure
  p >>= f  = foldProgress Step Fail f p

instance MonadFail (Progress step String) where
  fail = Fail

instance Applicative (Progress step fail) where
  pure a  = Done a
  p <*> x = foldProgress Step Fail (flip fmap x) p

instance Monoid fail => Alternative (Progress step fail) where
  empty   = Fail mempty
  p <|> q = foldProgress Step (const q) Done p
