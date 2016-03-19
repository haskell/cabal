{-# LANGUAGE CPP #-}
-- | Conflict sets
--
-- Intended for double import
--
-- > import Distribution.Client.Dependency.Modular.ConflictSet (ConflictSet)
-- > import qualified Distribution.Client.Dependency.Modular.ConflictSet as CS
module Distribution.Client.Dependency.Modular.ConflictSet (
    ConflictSet -- opaque
  , showCS
    -- Set-like operations
  , toList
  , union
  , unions
  , insert
  , empty
  , singleton
  , member
  , filter
  , fromList
  ) where

import Prelude hiding (filter)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as S

import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Var

-- | The set of variables involved in a solver conflict
--
-- Since these variables should be preprocessed in some way, this type is
-- kept abstract.
newtype ConflictSet qpn = CS { fromConflictSet :: Set (Var qpn) }
  deriving (Eq, Ord, Show)

showCS :: ConflictSet QPN -> String
showCS = intercalate ", " . map showVar . toList

{-------------------------------------------------------------------------------
  Set-like operations
-------------------------------------------------------------------------------}

toList :: ConflictSet qpn -> [Var qpn]
toList = S.toList . fromConflictSet

union :: Ord qpn => ConflictSet qpn -> ConflictSet qpn -> ConflictSet qpn
union (CS a) (CS b) = CS (a `S.union` b)

unions :: Ord qpn => [ConflictSet qpn] -> ConflictSet qpn
unions = CS . S.unions . map fromConflictSet

insert :: Ord qpn => Var qpn -> ConflictSet qpn -> ConflictSet qpn
insert var (CS set) = CS (S.insert (simplifyVar var) set)

empty :: ConflictSet qpn
empty = CS S.empty

singleton :: Var qpn -> ConflictSet qpn
singleton = CS . S.singleton . simplifyVar

member :: Ord qpn => Var qpn -> ConflictSet qpn -> Bool
member var (CS set) = S.member (simplifyVar var) set

#if MIN_VERSION_containers(0,5,0)
filter :: (Var qpn -> Bool) -> ConflictSet qpn -> ConflictSet qpn
#else
filter :: Ord qpn => (Var qpn -> Bool) -> ConflictSet qpn -> ConflictSet qpn
#endif
filter p (CS set) = CS $ S.filter p set

fromList :: Ord qpn => [Var qpn] -> ConflictSet qpn
fromList = CS . S.fromList . map simplifyVar
