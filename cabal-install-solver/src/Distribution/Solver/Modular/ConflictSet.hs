-- | Conflict sets
--
-- Intended for double import
--
-- > import Distribution.Solver.Modular.ConflictSet (ConflictSet)
-- > import qualified Distribution.Solver.Modular.ConflictSet as CS
module Distribution.Solver.Modular.ConflictSet (
    ConflictSet -- opaque
  , Conflict(..)
  , ConflictMap
  , OrderedVersionRange(..)
  , showConflictSet
  , showCSSortedByFrequency
  , showCSWithFrequency
    -- Set-like operations
  , toSet
  , toList
  , union
  , unions
  , insert
  , delete
  , empty
  , singleton
  , singletonWithConflict
  , size
  , member
  , lookup
  , filter
  , fromList
  ) where

import Prelude hiding (lookup)
import Data.List (intercalate, sortBy)
import Data.Map (Map)
import Data.Set (Set)
import Data.Function (on)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Distribution.Solver.Modular.Var
import Distribution.Solver.Modular.Version
import Distribution.Solver.Types.PackagePath

-- | The set of variables involved in a solver conflict, each paired with
-- details about the conflict.
newtype ConflictSet = CS {
    -- | The set of variables involved in the conflict
    conflictSetToMap :: Map (Var QPN) (Set Conflict)
  }
  deriving (Eq, Show)

-- | More detailed information about how a conflict set variable caused a
-- conflict. This information can be used to determine whether a second value
-- for that variable would lead to the same conflict.
--
-- TODO: Handle dependencies under flags or stanzas.
data Conflict =

    -- | The conflict set variable represents a package which depends on the
    -- specified problematic package. For example, the conflict set entry
    -- '(P x, GoalConflict y)' means that package x introduced package y, and y
    -- led to a conflict.
    GoalConflict QPN

    -- | The conflict set variable represents a package with a constraint that
    -- excluded the specified package and version. For example, the conflict set
    -- entry '(P x, VersionConstraintConflict y (mkVersion [2, 0]))' means that
    -- package x's constraint on y excluded y-2.0.
  | VersionConstraintConflict QPN Ver

    -- | The conflict set variable represents a package that was excluded by a
    -- constraint from the specified package. For example, the conflict set
    -- entry '(P x, VersionConflict y (orLaterVersion (mkVersion [2, 0])))'
    -- means that package y's constraint 'x >= 2.0' excluded some version of x.
  | VersionConflict QPN OrderedVersionRange

    -- | Any other conflict.
  | OtherConflict
  deriving (Eq, Ord, Show)

-- | Version range with an 'Ord' instance.
newtype OrderedVersionRange = OrderedVersionRange VR
  deriving (Eq, Show)

-- TODO: Avoid converting the version ranges to strings.
instance Ord OrderedVersionRange where
  compare = compare `on` show

showConflictSet :: ConflictSet -> String
showConflictSet = intercalate ", " . map showVar . toList

showCSSortedByFrequency :: ConflictMap -> ConflictSet -> String
showCSSortedByFrequency = showCS False

showCSWithFrequency :: ConflictMap -> ConflictSet -> String
showCSWithFrequency = showCS True

showCS :: Bool -> ConflictMap -> ConflictSet -> String
showCS showCount cm =
    intercalate ", " . map showWithFrequency . indexByFrequency
  where
    indexByFrequency = sortBy (flip compare `on` snd) . map (\c -> (c, M.lookup c cm)) . toList
    showWithFrequency (conflict, maybeFrequency) = case maybeFrequency of
      Just frequency
        | showCount -> showVar conflict ++ " (" ++ show frequency ++ ")"
      _             -> showVar conflict

{-------------------------------------------------------------------------------
  Set-like operations
-------------------------------------------------------------------------------}

toSet :: ConflictSet -> Set (Var QPN)
toSet = M.keysSet . conflictSetToMap

toList :: ConflictSet -> [Var QPN]
toList = M.keys . conflictSetToMap

union :: ConflictSet -> ConflictSet -> ConflictSet
union cs cs' = CS {
      conflictSetToMap = M.unionWith S.union (conflictSetToMap cs) (conflictSetToMap cs')
    }

unions :: [ConflictSet] -> ConflictSet
unions css = CS {
      conflictSetToMap = M.unionsWith S.union (map conflictSetToMap css)
    }

insert :: Var QPN -> ConflictSet -> ConflictSet
insert var cs = CS {
      conflictSetToMap = M.insert var (S.singleton OtherConflict) (conflictSetToMap cs)
    }

delete :: Var QPN -> ConflictSet -> ConflictSet
delete var cs = CS {
      conflictSetToMap = M.delete var (conflictSetToMap cs)
    }

empty :: ConflictSet
empty = CS {
      conflictSetToMap = M.empty
    }

singleton :: Var QPN -> ConflictSet
singleton var = singletonWithConflict var OtherConflict

singletonWithConflict :: Var QPN -> Conflict -> ConflictSet
singletonWithConflict var conflict = CS {
      conflictSetToMap = M.singleton var (S.singleton conflict)
    }

size :: ConflictSet -> Int
size = M.size . conflictSetToMap

member :: Var QPN -> ConflictSet -> Bool
member var = M.member var . conflictSetToMap

lookup :: Var QPN -> ConflictSet -> Maybe (Set Conflict)
lookup var = M.lookup var . conflictSetToMap

fromList :: [Var QPN] -> ConflictSet
fromList vars = CS {
      conflictSetToMap = M.fromList [(var, S.singleton OtherConflict) | var <- vars]
    }

type ConflictMap = Map (Var QPN) Int
