-- |
--
-- Module      :  Distribution.Deprecated.ReadP
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- Mostly re-exports of 'Text.ParserCombinators.ReadP', with added type synonym
-- 'Parser', and added functions 'skipSpaces1' and 'readP_to_E'.
module Distribution.Deprecated.ReadP
  ( -- * The 'ReadP' type
    T.ReadP

    -- * Primitive operations
  , T.get
  , T.look
  , (T.+++)
  , (T.<++)
  , T.gather

    -- * Other operations
  , T.pfail
  , T.eof
  , T.satisfy
  , T.char
  , T.string
  , T.munch
  , T.munch1
  , T.skipSpaces
  , skipSpaces1
  , T.choice
  , T.count
  , T.between
  , T.option
  , T.optional
  , T.many
  , T.many1
  , T.skipMany
  , T.skipMany1
  , T.sepBy
  , T.sepBy1
  , T.endBy
  , T.endBy1
  , T.chainr
  , T.chainl
  , T.chainl1
  , T.chainr1
  , T.manyTill

    -- * Running a parser
  , T.ReadS
  , T.readP_to_S
  , T.readS_to_P
  , readP_to_E

    -- ** Internal
  , Parser
  )
where

import Distribution.Client.Compat.Prelude
import Distribution.ReadE (ReadE (..))
import qualified Text.ParserCombinators.ReadP as T

type Parser = T.ReadP

skipSpaces1 :: T.ReadP ()
-- ^ Like 'skipSpaces' but succeeds only if there is at least one
-- whitespace character to skip.
skipSpaces1 = T.satisfy isSpace >> T.skipSpaces

readP_to_E :: (String -> String) -> T.ReadP a -> ReadE a
readP_to_E err r =
  ReadE $ \txt -> case [ p | (p, s) <- T.readP_to_S r txt, all isSpace s
                       ] of
    [] -> Left (err txt)
    (p : _) -> Right p
