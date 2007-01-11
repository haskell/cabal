{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CabalSetup
-- Copyright   :  (c) The University of Glasgow 2006
-- 
-- Maintainer  :  http://hackage.haskell.org/trac/hackage
-- Stability   :  alpha
-- Portability :  portable
--
-- The user interface to building and installing Cabal packages.

module Main (main) where

import Distribution.SetupWrapper
import System.Environment

main = do 
  args <- getArgs
  setupWrapper args Nothing
