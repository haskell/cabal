{-# OPTIONS_HADDOCK hide #-}

module Distribution.Deprecated.ProjectParseUtils
  ( ProjectParseResult (..)
  , projectParseFail
  , projectParse
  ) where

import Distribution.Client.Compat.Prelude hiding (get)
import Prelude ()

import Distribution.Deprecated.ParseUtils
import Distribution.Solver.Types.ProjectConfigPath (ProjectConfigPath)

data ProjectParseResult a
  = ProjectParseFailed (Maybe ProjectConfigPath, PError)
  | ProjectParseOk [(ProjectConfigPath, PWarning)] a
  deriving (Show)

projectParse :: ProjectConfigPath -> ParseResult a -> ProjectParseResult a
projectParse path (ParseFailed err) = ProjectParseFailed (Just path, err)
projectParse path (ParseOk ws x) = ProjectParseOk [(path, w) | w <- ws] x

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

projectParseFail :: Maybe ProjectConfigPath -> PError -> ProjectParseResult a
projectParseFail = curry ProjectParseFailed
