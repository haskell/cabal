{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Solver.Types.Settings
    ( FindBestSolution(..)
    , ReorderGoals(..)
    , IndependentGoals(..)
    , AvoidReinstalls(..)
    , ShadowPkgs(..)
    , StrongFlags(..)
    , AllowBootLibInstalls(..)
    , EnableBackjumping(..)
    , CountConflicts(..)
    , SolveExecutables(..)
    , InstallPlanScore(..)
    , showInstallPlanScore
    , defaultInstallPlanScore
    ) where

import Distribution.Simple.Setup ( BooleanFlag(..) )
import Distribution.Compat.Binary (Binary(..))
import GHC.Generics (Generic)

newtype FindBestSolution = FindBestSolution Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype ReorderGoals = ReorderGoals Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype CountConflicts = CountConflicts Bool
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

newtype EnableBackjumping = EnableBackjumping Bool
  deriving (BooleanFlag, Eq, Generic, Show)

newtype SolveExecutables = SolveExecutables Bool
  deriving (BooleanFlag, Eq, Generic, Show)

instance Binary FindBestSolution
instance Binary ReorderGoals
instance Binary CountConflicts
instance Binary IndependentGoals
instance Binary AvoidReinstalls
instance Binary ShadowPkgs
instance Binary StrongFlags
instance Binary AllowBootLibInstalls
instance Binary SolveExecutables

newtype InstallPlanScore = InstallPlanScore { unInstallPlanScore :: Double }
  deriving (Eq, Ord, Num, Fractional, Generic, Show)

instance Binary InstallPlanScore

showInstallPlanScore :: InstallPlanScore -> String
showInstallPlanScore (InstallPlanScore x) = show x

-- | Placeholder used when no score is calculated, e.g., the score assigned by
-- the Topdown solver.
defaultInstallPlanScore :: InstallPlanScore
defaultInstallPlanScore = 0
