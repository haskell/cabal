{-# LANGUAGE DeriveGeneric #-}
module Distribution.Client.Types.RepoName (
    RepoName (..),
    unRepoName,
) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.FieldGrammar.Described (Described (..), csAlphaNum, reMunch1CS)
import Distribution.Parsec                 (Parsec (..))
import Distribution.Pretty                 (Pretty (..))

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint                as Disp

-- | Repository name.
--
-- May be used as path segment.
--
newtype RepoName = RepoName String
  deriving (Show, Eq, Ord, Generic)

unRepoName :: RepoName -> String
unRepoName (RepoName n) = n

instance Binary RepoName
instance Structured RepoName
instance NFData RepoName

instance Pretty RepoName where
    pretty = Disp.text . unRepoName

instance Parsec RepoName where
    parsec = RepoName <$>
        P.munch1 (\c -> isAlphaNum c || c == '_' || c == '-' || c == '.')

instance Described RepoName where
    describe _ = reMunch1CS $ csAlphaNum <> fromString "_-."
