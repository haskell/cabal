-- to suppress WARNING in "Distribution.Compat.Prelude.Internal"
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | This module does two things:
--
-- * Acts as a compatibility layer, like @base-compat@.
--
-- * Provides commonly used imports.
--
-- This module is a superset of "Distribution.Compat.Prelude" (which
-- this module re-exports)
--
module Distribution.Client.Compat.Prelude
  ( module Distribution.Compat.Prelude.Internal
  , Prelude.IO
  , Proxy (..)
  ) where

import Prelude (IO)
import Distribution.Compat.Prelude.Internal hiding (IO)
import Data.Proxy (Proxy (..))
