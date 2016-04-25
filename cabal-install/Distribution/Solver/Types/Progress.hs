{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
module Distribution.Solver.Types.Progress
    ( Progress(..)
    , foldProgress
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
         ( Applicative(..) )
#endif
import Control.Applicative
         ( Alternative(..) )

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
         ( Monoid(..) )
#endif

import Prelude hiding (fail)

-- | A type to represent the unfolding of an expensive long running
-- calculation that may fail. We may get intermediate steps before the final
-- result which may be used to indicate progress and\/or logging messages.
--
data Progress step fail done = Step step (Progress step fail done)
                             | Fail fail
                             | Done done
  deriving (Functor)

-- | Consume a 'Progress' calculation. Much like 'foldr' for lists but with two
-- base cases, one for a final result and one for failure.
--
-- Eg to convert into a simple 'Either' result use:
--
-- > foldProgress (flip const) Left Right
--
foldProgress :: (step -> a -> a) -> (fail -> a) -> (done -> a)
             -> Progress step fail done -> a
foldProgress step fail done = fold
  where fold (Step s p) = step s (fold p)
        fold (Fail f)   = fail f
        fold (Done r)   = done r

instance Monad (Progress step fail) where
  return   = pure
  p >>= f  = foldProgress Step Fail f p

instance Applicative (Progress step fail) where
  pure a  = Done a
  p <*> x = foldProgress Step Fail (flip fmap x) p

instance Monoid fail => Alternative (Progress step fail) where
  empty   = Fail mempty
  p <|> q = foldProgress Step (const q) Done p
