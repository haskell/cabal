{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Solver.Types.Settings
    ( ReorderGoals(..)
    , IndependentGoals(..)
    , MinimizeConflictSet(..)
    , AvoidReinstalls(..)
    , ShadowPkgs(..)
    , StrongFlags(..)
    , AllowBootLibInstalls(..)
    , OnlyConstrained(..)
    , EnableBackjumping(..)
    , CountConflicts(..)
    , SolveExecutables(..)
    ) where

import Distribution.Simple.Setup ( BooleanFlag(..) )
import Distribution.Compat.Binary (Binary(..))
import Distribution.Pretty ( Pretty(pretty) )
import Distribution.Deprecated.Text ( Text(parse) )
import GHC.Generics (Generic)

import qualified Distribution.Deprecated.ReadP as Parse
import qualified Text.PrettyPrint as PP

newtype ReorderGoals = ReorderGoals Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype CountConflicts = CountConflicts Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype MinimizeConflictSet = MinimizeConflictSet Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype IndependentGoals = IndependentGoals Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype AvoidReinstalls = AvoidReinstalls Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype ShadowPkgs = ShadowPkgs Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype StrongFlags = StrongFlags Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype AllowBootLibInstalls = AllowBootLibInstalls Bool
  deriving (BooleanFlag, Eq, Generic, Show)

-- | Should we consider all packages we know about, or only those that
-- have constraints explicitly placed on them or which are goals?
data OnlyConstrained
  = OnlyConstrainedNone
  | OnlyConstrainedAll
  deriving (Eq, Generic, Show)

newtype EnableBackjumping = EnableBackjumping Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype SolveExecutables = SolveExecutables Bool
  deriving (BooleanFlag, Eq, Generic, Show)

instance Binary ReorderGoals
instance Binary CountConflicts
instance Binary IndependentGoals
instance Binary MinimizeConflictSet
instance Binary AvoidReinstalls
instance Binary ShadowPkgs
instance Binary StrongFlags
instance Binary AllowBootLibInstalls
instance Binary OnlyConstrained
instance Binary SolveExecutables

instance Pretty OnlyConstrained where
  pretty OnlyConstrainedAll = PP.text "all"
  pretty OnlyConstrainedNone = PP.text "none"

instance Text OnlyConstrained where
  parse = Parse.choice
    [ Parse.string "all" >> return OnlyConstrainedAll
    , Parse.string "none" >> return OnlyConstrainedNone
    ]

