{-# LANGUAGE CPP #-}
module Distribution.Client.Utils.Assertion (debugAssert) where

#ifdef DEBUG_ASSERTIONS
import Control.Exception (assert)
import Distribution.Compat.Stack
#endif

-- | Like 'assert', but only enabled with -fdebug-assertions. This function can
-- be used for expensive assertions that should only be turned on during testing
-- or debugging.
#ifdef DEBUG_ASSERTIONS
debugAssert :: WithCallStack (Bool -> a -> a)
debugAssert = assert
#else
debugAssert :: Bool -> a -> a
debugAssert _ = id
#endif
