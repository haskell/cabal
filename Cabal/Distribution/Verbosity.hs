{-# LANGUAGE DeriveGeneric #-}

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

module Distribution.Verbosity
    ( -- * Verbosity
      Verbosity, silent, normal, verbose, deafening
    , moreVerbose, lessVerbose
    , intToVerbosity, flagToVerbosity
    , showForCabal, showForGHC
    , fromVerbosityFlag
    ) where

import Distribution.Compat.Binary (Binary)
import Distribution.Flag
import Distribution.ReadE

import Data.List (elemIndex)
import GHC.Generics (Generic)

data Verbosity = Silent | Normal | Verbose | Deafening
    deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded)

instance Binary Verbosity

-- | Print /nothing/ unless an error occurs. silent mode
silent :: Verbosity
silent = Silent

-- | Print messages that should be shown by default.
normal :: Verbosity
normal = Normal

-- | Print messages more verbosely than 'normal'.
verbose :: Verbosity
verbose = Verbose

-- | Be more verbose than 'verbose' /and/ tell child processes to be
-- verbose, too.
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

-- | Given an accessor, get a definite 'Verbosity' from a 'Flag'.
fromVerbosityFlag :: (a -> Flag Verbosity) -> a -> Verbosity
fromVerbosityFlag get = fromFlagOrDefault normal . get
