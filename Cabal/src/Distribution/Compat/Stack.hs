{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Compat.Stack
  ( WithCallStack
  , CallStack
  , withFrozenCallStack
  , withLexicalCallStack
  , callStack
  , prettyCallStack
  , parentSrcLocPrefix
  ) where

import GHC.Stack

type WithCallStack a = HasCallStack => a

-- | Give the *parent* of the person who invoked this;
-- so it's most suitable for being called from a utility function.
-- You probably want to call this using 'withFrozenCallStack'; otherwise
-- it's not very useful.
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
