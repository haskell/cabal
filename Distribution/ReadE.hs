-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.ReadE
-- Copyright   :  Jose Iborra 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Simple parsing with failure

{- Copyright (c) 2007, Jose Iborra
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.ReadE (
   -- * ReadE
   ReadE(..), succeedReadE, failReadE,
   -- * Projections
   parseReadE, readEOrFail,
   readP_to_E
  ) where

import Distribution.Compat.ReadP
import Data.Char ( isSpace )

-- | Parser with simple error reporting
newtype ReadE a = ReadE {runReadE :: String -> Either ErrorMsg a}
type ErrorMsg   = String

instance Functor ReadE where
  fmap f (ReadE p) = ReadE $ \txt -> case p txt of
                                       Right a  -> Right (f a)
                                       Left err -> Left err

succeedReadE :: (String -> a) -> ReadE a
succeedReadE f = ReadE (Right . f)

failReadE :: ErrorMsg -> ReadE a
failReadE = ReadE . const Left

parseReadE :: ReadE a -> ReadP r a
parseReadE (ReadE p) = do
  txt <- look
  either fail return (p txt)

readEOrFail :: ReadE a -> (String -> a)
readEOrFail r = either error id . runReadE r

readP_to_E :: (String -> ErrorMsg) -> ReadP a a -> ReadE a
readP_to_E err r =
    ReadE $ \txt -> case [ p | (p, s) <- readP_to_S r txt
                         , all isSpace s ]
                    of [] -> Left (err txt)
                       (p:_) -> Right p
