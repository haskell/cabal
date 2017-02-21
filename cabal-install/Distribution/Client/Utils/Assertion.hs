{-# LANGUAGE CPP #-}
module Distribution.Client.Utils.Assertion (debugAssert) where

#ifdef DEBUG_ASSERTIONS
import Control.Exception (assert)
#endif

import Distribution.Compat.Stack

-- | Like 'assert', but only enabled with -fdebug-assertions. This function can
-- be used for expensive assertions that should only be turned on during testing
-- or debugging.
debugAssert :: WithCallStack (Bool -> a -> a)
#ifdef DEBUG_ASSERTIONS
debugAssert = assert
#else
debugAssert _ = id
#endif
