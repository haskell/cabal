{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Types.WriteGhcEnvironmentFilesPolicy
  ( WriteGhcEnvironmentFilesPolicy (..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

-- | Whether 'v2-build' should write a .ghc.environment file after
-- success. Possible values: 'always', 'never' (the default), 'ghc8.4.4+'
-- (8.4.4 is the earliest version that supports
-- '-package-env -').
data WriteGhcEnvironmentFilesPolicy
  = AlwaysWriteGhcEnvironmentFiles
  | NeverWriteGhcEnvironmentFiles
  | WriteGhcEnvironmentFilesOnlyForGhc844AndNewer
  deriving (Eq, Enum, Bounded, Generic, Show)

instance Binary WriteGhcEnvironmentFilesPolicy
instance Structured WriteGhcEnvironmentFilesPolicy
