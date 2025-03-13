{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Compat.Stack
  ( WithCallStack
  , CallStack
  , annotateCallStackIO
  , withFrozenCallStack
  , withLexicalCallStack
  , callStack
  , prettyCallStack
  , parentSrcLocPrefix
  ) where

import GHC.Stack
import System.IO.Error

type WithCallStack a = HasCallStack => a

-- | Give the *parent* of the person who invoked this;
-- so it's most suitable for being called from a utility function.
-- You probably want to call this using 'withFrozenCallStack'; otherwise
-- it's not very useful.  We didn't implement this for base-4.8.1
-- because we cannot rely on freezing to have taken place.
parentSrcLocPrefix :: WithCallStack String
parentSrcLocPrefix =
  case getCallStack callStack of
    (_ : (_, loc) : _) -> showLoc loc
    [(_, loc)] -> showLoc loc
    [] -> error "parentSrcLocPrefix: empty call stack"
  where
    showLoc loc =
      srcLocFile loc ++ ":" ++ show (srcLocStartLine loc) ++ ": "

-- Yeah, this uses skivvy implementation details.
withLexicalCallStack :: (a -> WithCallStack (IO b)) -> WithCallStack (a -> IO b)
withLexicalCallStack f =
  let stk = ?callStack
   in \x -> let ?callStack = stk in f x

-- | This function is for when you *really* want to add a call
-- stack to raised IO, but you don't have a
-- 'Distribution.Verbosity.Verbosity' so you can't use
-- 'Distribution.Simple.Utils.annotateIO'.  If you have a 'Verbosity',
-- please use that function instead.
annotateCallStackIO :: WithCallStack (IO a -> IO a)
annotateCallStackIO = modifyIOError f
  where
    f ioe =
      ioeSetErrorString ioe
        . wrapCallStack
        $ ioeGetErrorString ioe
    wrapCallStack s =
      prettyCallStack callStack ++ "\n" ++ s
