-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Verbosity
-- Copyright   :  Ian Lynagh 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A simple 'Verbosity' type with associated utilities. There are 4 standard
-- verbosity levels from 'silent', 'normal', 'verbose' up to 'deafening'. This
-- is used for deciding what logging messages to print.

-- Verbosity for Cabal functions.

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
