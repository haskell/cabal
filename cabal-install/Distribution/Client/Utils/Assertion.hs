{-# LANGUAGE CPP #-}
module Distribution.Client.Utils.Assertion (
  expensiveAssert,
  pprAssert
) where

import Distribution.Verbosity
import Distribution.Outputable
import Control.Exception (assert, throw, AssertionFailed(..))
import Distribution.Compat.Stack

-- | Like 'assert', but only enabled with -fdebug-expensive-assertions. This
-- function can be used for expensive assertions that should only be turned on
-- during testing or debugging.
#ifdef DEBUG_EXPENSIVE_ASSERTIONS
expensiveAssert :: WithCallStack (Bool -> a -> a)
expensiveAssert = assert
#else
expensiveAssert :: Bool -> a -> a
expensiveAssert _ = id
#endif

callStackDoc :: WithCallStack SDoc
callStackDoc =
    hang (text "Call stack:")
       4 (vcat $ map text $ lines (prettyCallStack callStack))

panicDoc :: String -> SDoc -> a
-- TODO: Either take verbosity as an argument here (threading it
-- through), or unsafely stash it in a global
panicDoc x doc = throw (AssertionFailed (x ++ "\n\n" ++ showSDoc normal doc))

pprPanic :: WithCallStack (String -> SDoc -> a)
pprPanic s doc = panicDoc s (doc $$ callStackDoc)

assertPprPanic :: WithCallStack (SDoc -> a)
assertPprPanic doc = pprPanic "ASSERT failed!" doc

-- NB: This is a little hack to make sure we respect -fignore-assert,
-- while taking over the actual assert generation so we can put in the
-- information we care about.  We are throwing away file/line
-- information, but we can recover this information from the call stack.
pprAssert :: WithCallStack (Bool -> SDoc -> a -> a)
pprAssert b doc r = withFrozenCallStack (assert (if b then True else assertPprPanic doc) r)
