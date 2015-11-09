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
  , ConflictMap
#ifdef DEBUG_CONFLICT_SETS
  , conflictSetOrigin
#endif
  , ConflictType(..)
  , showConflictSet
  , showCSSortedByFrequency
  , showCSWithFrequency
    -- Set-like operations
  , toList
  , union
  , unions
  , insert
  , insertWithConflictType
  , empty
  , singleton
  , member
  , lookup
  , filter
  , fromList
  ) where

import Prelude hiding (filter, lookup)
import Data.List (intercalate, sortBy)
import Distribution.Compat.Map.Strict (Map)
import Data.Function (on)
import qualified Distribution.Compat.Map.Strict as M

#ifdef DEBUG_CONFLICT_SETS
import Data.Tree
import GHC.Stack
#endif

import Distribution.Solver.Modular.Var
import Distribution.Solver.Types.PackagePath

-- | The set of variables involved in a solver conflict
--
-- Since these variables should be preprocessed in some way, this type is
-- kept abstract.
data ConflictSet = CS {
    -- | The set of variables involved on the conflict
    conflictSetToMap :: Map (Var QPN) ConflictType

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

instance Eq ConflictSet where
  (==) = (==) `on` conflictSetToMap

instance Ord ConflictSet where
  compare = compare `on` conflictSetToMap

-- TODO: The name of this type and its constructors could be improved.
data ConflictType =

  -- | Any other value in the variable's domain might resolve the conflict.
  ConflictAll

  -- | Only values that are less than the current assignment can resolve the
  -- conflict.
  | ConflictLessThan
  deriving (Eq, Ord, Show)

combineConflictType :: ConflictType -> ConflictType -> ConflictType
combineConflictType ConflictLessThan ConflictLessThan = ConflictLessThan
combineConflictType _ _ = ConflictAll

showConflictSet :: ConflictSet -> String
showConflictSet =
    intercalate ", " . map (uncurry showConflict) . M.toList . conflictSetToMap
  where
    -- TODO: How should we display the type of conflict?
    showConflict v t = "(" ++ showVar v ++ ", " ++ show t ++ ")"

showCSSortedByFrequency :: ConflictMap -> ConflictSet -> String
showCSSortedByFrequency = showCS False

showCSWithFrequency :: ConflictMap -> ConflictSet -> String
showCSWithFrequency = showCS True

showCS :: Bool -> ConflictMap -> ConflictSet -> String
showCS showCount cm =
    intercalate ", " . map showWithFrequency . indexByFrequency . toList
  where
    indexByFrequency = sortBy (flip compare `on` snd) . map (\c -> (c, M.lookup c cm))
    showWithFrequency (conflict, maybeFrequency) = case maybeFrequency of
      Just frequency
        | showCount -> showVar conflict ++ " (" ++ show frequency ++ ")"
      _             -> showVar conflict

{-------------------------------------------------------------------------------
  Set-like operations
-------------------------------------------------------------------------------}

toList :: ConflictSet -> [Var QPN]
toList = map fst . M.toList . conflictSetToMap

union ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  ConflictSet -> ConflictSet -> ConflictSet
union cs cs' = CS {
      conflictSetToMap = M.unionWith combineConflictType
                         (conflictSetToMap cs) (conflictSetToMap cs')
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc (map conflictSetOrigin [cs, cs'])
#endif
    }

unions ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  [ConflictSet] -> ConflictSet
unions css = CS {
      conflictSetToMap = M.unionsWith combineConflictType
                         (map conflictSetToMap css)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc (map conflictSetOrigin css)
#endif
    }

insert :: Var QPN -> ConflictSet -> ConflictSet
insert v = insertWithConflictType v ConflictAll

insertWithConflictType ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Var QPN -> ConflictType -> ConflictSet -> ConflictSet
insertWithConflictType var ct cs = CS {
      conflictSetToMap = M.insertWith combineConflictType
                         (simplifyVar var) ct (conflictSetToMap cs)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc [conflictSetOrigin cs]
#endif
    }

empty ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  ConflictSet
empty = CS {
      conflictSetToMap = M.empty
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc []
#endif
    }

singleton ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Var QPN -> ConflictSet
singleton var = CS {
      conflictSetToMap = M.singleton (simplifyVar var) ConflictAll
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc []
#endif
    }

member :: Var QPN -> ConflictSet -> Bool
member var = M.member (simplifyVar var) . conflictSetToMap

lookup :: Var QPN -> ConflictSet -> Maybe ConflictType
lookup var = M.lookup (simplifyVar var) . conflictSetToMap

filter ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  (Var QPN -> Bool) -> ConflictSet -> ConflictSet
filter p cs = CS {
      conflictSetToMap = M.filterWithKey (\v _ -> p v) (conflictSetToMap cs)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc [conflictSetOrigin cs]
#endif
    }

fromList ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  [Var QPN] -> ConflictSet
fromList vars = CS {
      conflictSetToMap = M.fromListWith combineConflictType
         $ map (\v -> (simplifyVar v, ConflictAll)) vars
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc []
#endif
    }

type ConflictMap = Map (Var QPN) Int

