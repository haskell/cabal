module PartialValuation where

import Data.Monoid
         ( Monoid(..) )
import Control.Monad
         ( liftM, liftM2 )
import qualified Data.Foldable as Foldable
         ( Foldable(foldMap, foldr), toList )

import Prelude hiding (null)

-- | A valuation on some base predicate type.
--
type TotalValuation base = (base -> Bool)

-- | A partial valuation on some base predicate. This is useful when we have
-- only partial information but still wish to simplify an expression using the
-- predicates.
--
-- In particular, a partial valuation is a 'Monoid' via 'null' and 'extend'.
--
newtype PartialValuation base
      = PartialValuation { applyPartialValuation :: base -> Maybe Bool }

-- | A null valuation. It gives no value for all predicates.
--
null :: PartialValuation base
null = PartialValuation (const Nothing)

-- | Extend one partial valuation with another.
--
-- In the valuation @v' `extend` v@, the valuation @v@ is being extended by the
-- valuation @v'@. That means values returned by the first take precedence over
-- those returned by the second.
--
extend :: PartialValuation base
       -> PartialValuation base
       -> PartialValuation base
extend (PartialValuation v) (PartialValuation v') =
    PartialValuation $ \x -> v x `override` v' x
  where
    override a@(Just _) _ = a
    override _          b = b

instance Monoid (PartialValuation base) where
  mempty  = null
  mappend = extend


-- | Extend one a total valuation with a partial valuation to give a total
-- valuation.
--
-- Values from the partial valuation take precedence.
--
extend' :: PartialValuation base
        -> TotalValuation base
        -> TotalValuation base
extend' (PartialValuation v) v' =
  \x -> v x `override` v' x
  where
    override (Just a) _ = a
    override _        b = b
