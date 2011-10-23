-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Verbosity
-- Copyright   :  Ian Lynagh 2007
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A simple 'Verbosity' type with associated utilities. There are 4 standard
-- verbosity levels from 'silent', 'normal', 'verbose' up to 'deafening'. This
-- is used for deciding what logging messages to print.

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
  moreVerbose, lessVerbose,
  intToVerbosity, flagToVerbosity,
  showForCabal, showForGHC
 ) where

import Data.List (elemIndex)
import Distribution.ReadE

data Verbosity = Silent | Normal | Verbose | Deafening
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- We shouldn't print /anything/ unless an error occurs in silent mode
silent :: Verbosity
silent = Silent

-- Print stuff we want to see by default
normal :: Verbosity
normal = Normal

-- Be more verbose about what's going on
verbose :: Verbosity
verbose = Verbose

-- Not only are we verbose ourselves (perhaps even noisier than when
-- being "verbose"), but we tell everything we run to be verbose too
deafening :: Verbosity
deafening = Deafening

moreVerbose :: Verbosity -> Verbosity
moreVerbose Silent    = Silent    --silent should stay silent
moreVerbose Normal    = Verbose
moreVerbose Verbose   = Deafening
moreVerbose Deafening = Deafening

lessVerbose :: Verbosity -> Verbosity
lessVerbose Deafening = Deafening
lessVerbose Verbose   = Normal
lessVerbose Normal    = Silent
lessVerbose Silent    = Silent

intToVerbosity :: Int -> Maybe Verbosity
intToVerbosity 0 = Just Silent
intToVerbosity 1 = Just Normal
intToVerbosity 2 = Just Verbose
intToVerbosity 3 = Just Deafening
intToVerbosity _ = Nothing

flagToVerbosity :: ReadE Verbosity
flagToVerbosity = ReadE $ \s ->
   case reads s of
       [(i, "")] ->
           case intToVerbosity i of
               Just v -> Right v
               Nothing -> Left ("Bad verbosity: " ++ show i ++
                                     ". Valid values are 0..3")
       _ -> Left ("Can't parse verbosity " ++ s)

showForCabal, showForGHC :: Verbosity -> String

showForCabal v = maybe (error "unknown verbosity") show $
    elemIndex v [silent,normal,verbose,deafening]
showForGHC   v = maybe (error "unknown verbosity") show $
    elemIndex v [silent,normal,__,verbose,deafening]
        where __ = silent -- this will be always ignored by elemIndex
