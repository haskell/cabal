{-# OPTIONS_HADDOCK hide #-}

module Distribution.Deprecated.ProjectParseUtils
  ( ProjectParseError
  , ProjectParseWarning
  , ProjectParseResult (..)
  , projectParseFail
  , projectParse
  ) where

import Distribution.Client.Compat.Prelude hiding (get)
import Prelude ()

import qualified Distribution.Deprecated.ParseUtils as Pkg (PError, PWarning, ParseResult (..))
import Distribution.Solver.Types.ProjectConfigPath (ProjectConfigPath)

type ProjectParseError = ((Maybe ProjectConfigPath, Pkg.PError), Maybe String)
type ProjectParseWarning = (ProjectConfigPath, Pkg.PWarning)

data ProjectParseResult a
  = ProjectParseFailed ProjectParseError
  | ProjectParseOk [ProjectParseWarning] a
  deriving (Show)

projectParse :: Maybe String -> ProjectConfigPath -> Pkg.ParseResult a -> ProjectParseResult a
projectParse s path (Pkg.ParseFailed err) = ProjectParseFailed ((Just path, err), s)
projectParse _ path (Pkg.ParseOk ws x) = ProjectParseOk [(path, w) | w <- ws] x

instance Functor ProjectParseResult where
  fmap _ (ProjectParseFailed err) = ProjectParseFailed err
  fmap f (ProjectParseOk ws x) = ProjectParseOk ws $ f x

instance Applicative ProjectParseResult where
  pure = ProjectParseOk []
  (<*>) = ap

instance Monad ProjectParseResult where
  return = pure
  ProjectParseFailed err >>= _ = ProjectParseFailed err
  ProjectParseOk ws x >>= f = case f x of
    ProjectParseFailed err -> ProjectParseFailed err
    ProjectParseOk ws' x' -> ProjectParseOk (ws' ++ ws) x'

projectParseFail :: Maybe String -> Maybe ProjectConfigPath -> Pkg.PError -> ProjectParseResult a
projectParseFail s p e = ProjectParseFailed ((p, e), s)
