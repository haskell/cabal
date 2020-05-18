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
    defaultApplicationDir
  , defaultSourceDir
  , defaultCabalVersion
  , myLibModule
  ) where

import Prelude (String)

import Distribution.ModuleName
  ( ModuleName )  -- And for the Text instance
import qualified Distribution.ModuleName as ModuleName
  ( fromString )
import Distribution.CabalSpecVersion
  ( CabalSpecVersion (..))

defaultApplicationDir :: String
defaultApplicationDir = "app"

defaultSourceDir :: String
defaultSourceDir = "src"

defaultCabalVersion :: CabalSpecVersion
defaultCabalVersion = CabalSpecV2_4

myLibModule :: ModuleName
myLibModule = ModuleName.fromString "MyLib"
