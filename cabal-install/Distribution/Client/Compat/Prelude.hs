-- | This module does two things:
--
-- * Acts as a compatiblity layer, like @base-compat@.
--
-- * Provides commonly used imports.
--
-- This module is a superset of "Distribution.Compat.Prelude" (which
-- this module re-exports)
--
module Distribution.Client.Compat.Prelude
  ( module Distribution.Compat.Prelude
  , Prelude.IO
  ) where

import Prelude (IO)
import Distribution.Compat.Prelude hiding (IO)
