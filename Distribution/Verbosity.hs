{-# OPTIONS -cpp -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Verbosity
-- Copyright   :  Ian Lynagh 2007
--
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Verbosity for Cabal functions

{- Copyright (c) 2007, Ian Lynagh
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

module Distribution.Verbosity (
  -- * Verbosity
  Verbosity,
  silent, normal, verbose, deafening,
  intToVerbosity, flagToVerbosity
 ) where

data Verbosity = Silent | Normal | Verbose | Deafening
    deriving (Show, Eq, Ord)

silent :: Verbosity
silent = Silent

normal :: Verbosity
normal = Normal

verbose :: Verbosity
verbose = Verbose

deafening :: Verbosity
deafening = Verbose

intToVerbosity :: Int -> Maybe Verbosity
intToVerbosity 0 = Just Silent
intToVerbosity 1 = Just Normal
intToVerbosity 2 = Just Verbose
intToVerbosity 3 = Just Deafening
intToVerbosity _ = Nothing

flagToVerbosity :: Maybe String -> Verbosity
flagToVerbosity Nothing = verbose
flagToVerbosity (Just s)
 = case reads s of
       [(i, "")] ->
           case intToVerbosity i of
               Just v -> v
               Nothing -> error ("Bad verbosity " ++ show i)
       _ -> error ("Can't parse verbosity " ++ s)

