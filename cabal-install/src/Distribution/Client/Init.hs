-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Init
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Implementation of the 'cabal init' command, which creates an initial .cabal
-- file for a project.
--
-----------------------------------------------------------------------------

module Distribution.Client.Init (

    -- * Commands
    initCabal
  , incVersion

  ) where

import Distribution.Client.Init.Command
  ( initCabal, incVersion )
