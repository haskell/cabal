{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}

module Distribution.Solver.Types.Stage
  ( Stage (..)
  , showStage
  , stages
  , prevStage
  , nextStage
  , Staged (..)
  , tabulate
  , foldMapWithKey
  , always
  ) where

import Prelude (Enum (..))
import Distribution.Compat.Prelude
import qualified Distribution.Compat.CharParsing as P

import Data.Maybe (fromJust)
import GHC.Stack

import Distribution.Parsec (Parsec (..))
import Distribution.Pretty (Pretty (..))
import Distribution.Utils.Structured (Structured (..))
import qualified Text.PrettyPrint as Disp


data Stage
  = -- | -- The system where the build is running
    Build
  | -- | -- The system where the built artifacts will run
    Host
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance Binary Stage
instance Structured Stage

instance Pretty Stage where
  pretty = Disp.text . showStage

showStage :: Stage -> String
showStage Build = "build"
showStage Host = "host"

instance Parsec Stage where
  parsec = P.choice [
      Build <$ P.string "build",
      Host <$ P.string "host"
    ]

stages :: [Stage]
stages = [minBound .. maxBound]

prevStage :: Stage -> Stage
prevStage s | s == minBound = s
            | otherwise     = Prelude.pred s 
nextStage :: Stage -> Stage
nextStage s | s == maxBound = s
            | otherwise     = Prelude.succ s

-- TOOD: I think there is similar code for stanzas, compare.

newtype Staged a = Staged
  { getStage :: Stage -> a
  }
  deriving (Functor, Generic)
  deriving Applicative via ((->) Stage)

instance Eq a => Eq (Staged a) where
  lhs == rhs =
    all
      (\stage -> getStage lhs stage == getStage rhs stage)
      [minBound .. maxBound]

instance Show a => Show (Staged a) where
  showsPrec _ staged =
    showList
      [ (stage, getStage staged stage)
      | stage <- [minBound .. maxBound]
      ]

instance Foldable Staged where
  foldMap f (Staged gs) = foldMap (f . gs) [minBound..maxBound]

instance Traversable Staged where
  traverse f = fmap index . traverse (traverse f) . tabulate

instance Binary a => Binary (Staged a) where
  put staged = put (tabulate staged)
  -- TODO this could be done better I think
  get =  index <$> get

-- TODO: I have no idea if this is right
instance (Typeable a, Structured a) => Structured (Staged a) where
  structure _ = structure (Proxy :: Proxy [(Stage, a)])

tabulate :: Staged a -> [(Stage, a)]
tabulate staged =
  [ (stage, getStage staged stage)
  | stage <- [minBound .. maxBound]
  ]

index :: HasCallStack => [(Stage, a)] -> Staged a
index t = Staged (\s -> fromJust (lookup s t))

foldMapWithKey :: Monoid m => (Stage -> a -> m) -> Staged a -> m
foldMapWithKey f = foldMap (uncurry f) . tabulate

always :: a -> Staged a
always = Staged . const
