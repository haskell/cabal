{-# LANGUAGE CPP #-}
#ifdef DEBUG_CONFLICT_SETS
{-# LANGUAGE ImplicitParams #-}
#endif
-- | Conflict sets
--
-- Intended for double import
--
-- > import Distribution.Solver.Modular.ConflictSet (ConflictSet)
-- > import qualified Distribution.Solver.Modular.ConflictSet as CS
module Distribution.Solver.Modular.ConflictSet (
    ConflictSet -- opaque
#ifdef DEBUG_CONFLICT_SETS
  , conflictSetOrigin
#endif
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
import Data.Function (on)
import qualified Data.Set as S

#ifdef DEBUG_CONFLICT_SETS
import Data.Tree
import GHC.Stack
#endif

import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Var

-- | The set of variables involved in a solver conflict
--
-- Since these variables should be preprocessed in some way, this type is
-- kept abstract.
data ConflictSet qpn = CS {
    -- | The set of variables involved on the conflict
    conflictSetToSet :: Set (Var qpn)

#ifdef DEBUG_CONFLICT_SETS
    -- | The origin of the conflict set
    --
    -- When @DEBUG_CONFLICT_SETS@ is defined @(-f debug-conflict-sets)@,
    -- we record the origin of every conflict set. For new conflict sets
    -- ('empty', 'fromVars', ..) we just record the 'CallStack'; for operations
    -- that construct new conflict sets from existing conflict sets ('union',
    -- 'filter', ..)  we record the 'CallStack' to the call to the combinator
    -- as well as the 'CallStack's of the input conflict sets.
    --
    -- Requires @GHC >= 7.10@.
  , conflictSetOrigin :: Tree CallStack
#endif
  }
  deriving (Show)

instance Eq qpn => Eq (ConflictSet qpn) where
  (==) = (==) `on` conflictSetToSet

instance Ord qpn => Ord (ConflictSet qpn) where
  compare = compare `on` conflictSetToSet

showCS :: ConflictSet QPN -> String
showCS = intercalate ", " . map showVar . toList

{-------------------------------------------------------------------------------
  Set-like operations
-------------------------------------------------------------------------------}

toList :: ConflictSet qpn -> [Var qpn]
toList = S.toList . conflictSetToSet

union ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Ord qpn => ConflictSet qpn -> ConflictSet qpn -> ConflictSet qpn
union cs cs' = CS {
      conflictSetToSet = S.union (conflictSetToSet cs) (conflictSetToSet cs')
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc (map conflictSetOrigin [cs, cs'])
#endif
    }

unions ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Ord qpn => [ConflictSet qpn] -> ConflictSet qpn
unions css = CS {
      conflictSetToSet = S.unions (map conflictSetToSet css)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc (map conflictSetOrigin css)
#endif
    }

insert ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Ord qpn => Var qpn -> ConflictSet qpn -> ConflictSet qpn
insert var cs = CS {
      conflictSetToSet = S.insert (simplifyVar var) (conflictSetToSet cs)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc [conflictSetOrigin cs]
#endif
    }

empty ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  ConflictSet qpn
empty = CS {
      conflictSetToSet = S.empty
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc []
#endif
    }

singleton ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Var qpn -> ConflictSet qpn
singleton var = CS {
      conflictSetToSet = S.singleton (simplifyVar var)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc []
#endif
    }

member :: Ord qpn => Var qpn -> ConflictSet qpn -> Bool
member var = S.member (simplifyVar var) . conflictSetToSet

filter ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
#if !MIN_VERSION_containers(0,5,0)
  Ord qpn =>
#endif
  (Var qpn -> Bool) -> ConflictSet qpn -> ConflictSet qpn
filter p cs = CS {
      conflictSetToSet = S.filter p (conflictSetToSet cs)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc [conflictSetOrigin cs]
#endif
    }

fromList ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Ord qpn => [Var qpn] -> ConflictSet qpn
fromList vars = CS {
      conflictSetToSet = S.fromList (map simplifyVar vars)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc []
#endif
    }
