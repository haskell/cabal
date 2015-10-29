-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Display
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defines a 'Display' class which is a subclass of 'Text'. It adds
-- Parsec parser to the value.
--
-- The intention is to slowly deprecate 'parse' method of `Text'.
{-# LANGUAGE CPP #-}
module Distribution.Display (
  Display(..),
  simpleParsec,
  module Distribution.Text
  ) where

import Control.Applicative (Alternative(..))
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*))
#endif
import Distribution.Text
import Text.Parsec (ParsecT)
import qualified Text.Parsec as Parsec

class Text a => Display a where
  parsec :: Monad m => ParsecT String u m a

simpleParsec :: Display a => String -> Maybe a
simpleParsec str =
    case Parsec.parse parsecEof "input" str of
        Right res -> Just res
        Left _    -> Nothing
    where parsecEof = parsec <* Parsec.spaces <* Parsec.eof
-- -----------------------------------------------------------------------------
-- Instances for types from the base package

instance Display Bool where
    parsec = Parsec.choice [ (Parsec.string "True" <|>
                              Parsec.string "true") >> return True
                           , (Parsec.string "False" <|>
                              Parsec.string "false") >> return False ]

-- TODO: instance Display Version where
