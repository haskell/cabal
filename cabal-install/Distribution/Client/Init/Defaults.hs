-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Init.Defaults
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Default values to use in cabal init (if not specified in config/flags).
--
-----------------------------------------------------------------------------

module Distribution.Client.Init.Defaults (
    defaultCabalVersion
  , myLibModule
  ) where

import Distribution.ModuleName
  ( ModuleName )  -- And for the Text instance
import qualified Distribution.ModuleName as ModuleName
  ( fromString )
import Distribution.Version
  ( Version, mkVersion )

defaultCabalVersion :: Version
defaultCabalVersion = mkVersion [1,10]

myLibModule :: ModuleName
myLibModule = ModuleName.fromString "MyLib"
