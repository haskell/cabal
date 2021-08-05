-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Init.Utils
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Shared utilities used by multiple cabal init modules.
--
-----------------------------------------------------------------------------

module Distribution.Client.Init.Utils (
    eligibleForTestSuite
  , message
  ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Simple.Setup
  ( Flag(..) )
import Distribution.Client.Init.Types
  ( InitFlags(..), PackageType(..) )

-- | Returns true if this package is eligible for test suite initialization.
eligibleForTestSuite :: InitFlags -> Bool
eligibleForTestSuite flags =
  Flag True == initializeTestSuite flags
  && Flag Executable /= packageType flags

-- | Possibly generate a message to stdout, taking into account the
--   --quiet flag.
message :: InitFlags -> String -> IO ()
message (InitFlags{quiet = Flag True}) _ = return ()
message _ s = putStrLn s
