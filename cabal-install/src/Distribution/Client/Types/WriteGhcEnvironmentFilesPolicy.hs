{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Types.WriteGhcEnvironmentFilesPolicy
  ( WriteGhcEnvironmentFilesPolicy (..)
  ) where

import Distribution.Client.Compat.Prelude
import qualified Distribution.Compat.CharParsing as P
import Distribution.Parsec
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

instance Parsec WriteGhcEnvironmentFilesPolicy where
  parsec = do
    token <- parsecToken
    case token of
      "always" -> return AlwaysWriteGhcEnvironmentFiles
      "never" -> return NeverWriteGhcEnvironmentFiles
      "ghc8.4.4+" -> return WriteGhcEnvironmentFilesOnlyForGhc844AndNewer
      policy ->
        P.unexpected $
          "Cannot parse the GHC environment file write policy '"
            <> policy
            <> "'"
