{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Solver.Types.Settings
    ( ReorderGoals(..)
    , IndependentGoals(..)
    , PreferOldest(..)
    , MinimizeConflictSet(..)
    , AvoidReinstalls(..)
    , ShadowPkgs(..)
    , StrongFlags(..)
    , AllowBootLibInstalls(..)
    , OnlyConstrained(..)
    , EnableBackjumping(..)
    , CountConflicts(..)
    , FineGrainedConflicts(..)
    , SolveExecutables(..)
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Simple.Setup ( BooleanFlag(..) )
import Distribution.Pretty ( Pretty(pretty) )
import Distribution.Parsec ( Parsec(parsec) )

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as PP

newtype ReorderGoals = ReorderGoals Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype CountConflicts = CountConflicts Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype FineGrainedConflicts = FineGrainedConflicts Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype MinimizeConflictSet = MinimizeConflictSet Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype IndependentGoals = IndependentGoals Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype PreferOldest = PreferOldest Bool
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
instance Binary FineGrainedConflicts
instance Binary IndependentGoals
instance Binary PreferOldest
instance Binary MinimizeConflictSet
instance Binary AvoidReinstalls
instance Binary ShadowPkgs
instance Binary StrongFlags
instance Binary AllowBootLibInstalls
instance Binary OnlyConstrained
instance Binary SolveExecutables

instance Structured ReorderGoals
instance Structured CountConflicts
instance Structured FineGrainedConflicts
instance Structured IndependentGoals
instance Structured PreferOldest
instance Structured MinimizeConflictSet
instance Structured AvoidReinstalls
instance Structured ShadowPkgs
instance Structured StrongFlags
instance Structured AllowBootLibInstalls
instance Structured OnlyConstrained
instance Structured SolveExecutables

instance Pretty OnlyConstrained where
  pretty OnlyConstrainedAll  = PP.text "all"
  pretty OnlyConstrainedNone = PP.text "none"

instance Parsec OnlyConstrained where
  parsec = P.choice
    [ P.string "all"  >> return OnlyConstrainedAll
    , P.string "none" >> return OnlyConstrainedNone
    ]

instance Parsec ReorderGoals where
  parsec = ReorderGoals <$> parsec

instance Parsec CountConflicts where
  parsec = CountConflicts <$> parsec

instance Parsec FineGrainedConflicts where
  parsec = FineGrainedConflicts <$> parsec

instance Parsec MinimizeConflictSet where
  parsec = MinimizeConflictSet <$> parsec

instance Parsec StrongFlags where
  parsec = StrongFlags <$> parsec

instance Parsec AllowBootLibInstalls where
  parsec = AllowBootLibInstalls <$> parsec

instance Parsec PreferOldest where
  parsec = PreferOldest <$> parsec

instance Parsec IndependentGoals where
  parsec = IndependentGoals <$> parsec
