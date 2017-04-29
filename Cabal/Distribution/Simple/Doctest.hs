{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Haddock
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module deals with the @doctest@ command.

module Distribution.Simple.Doctest (
  doctest
  ) where

import Prelude ()
import Distribution.Compat.Prelude


-- local
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup
import Distribution.Simple.Build
import Distribution.Simple.LocalBuildInfo hiding (substPathTemplate)

import Distribution.Compat.Semigroup (Any (..))

-- -----------------------------------------------------------------------------
-- Types

-- | A record that represents the arguments to the doctest executable.
data DoctestArgs = DoctestArgs {
  argHelp :: Any,
  argVersion :: Any,
  argNoMagic :: Any
} deriving Generic

-- -----------------------------------------------------------------------------
-- Doctest support

doctest :: PackageDescription
        -> LocalBuildInfo
        -> [PPSuffixHandler]
        -> DoctestFlags
        -> IO ()
doctest pkg_descr lbi suffixes doctestFlags = do
  let verbosity = flag doctestVerbosity
      flag f    = fromFlag $ f doctestFlags

  withAllComponentsInBuildOrder pkg_descr lbi $ \component clbi -> do
     componentInitialBuildSteps (flag doctestDistPref) pkg_descr lbi clbi verbosity
     preprocessComponent pkg_descr component lbi clbi False verbosity suffixes
