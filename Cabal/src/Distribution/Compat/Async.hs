{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | 'Async', yet using 'MVar's.
--
-- Adopted from @async@ library
-- Copyright (c) 2012, Simon Marlow
-- Licensed under BSD-3-Clause
--
-- @since 3.2.0.0
module Distribution.Compat.Async
  ( AsyncM
  , withAsync
  , waitCatch
  , wait
  , asyncThreadId
  , cancel
  , uninterruptibleCancel
  , AsyncCancelled (..)

    -- * Cabal extras
  , withAsyncNF
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.DeepSeq (NFData, force)
import Control.Exception
  ( BlockedIndefinitelyOnMVar (..)
  , Exception (..)
  , SomeException (..)
  , catch
  , evaluate
  , mask
  , throwIO
  , throwTo
  , try
  , uninterruptibleMask_
  )
import Control.Monad (void)
import Data.Typeable (Typeable)
import GHC.Exts (inline)

#if MIN_VERSION_base(4,7,0)
import Control.Exception (asyncExceptionFromException, asyncExceptionToException)
#endif

-- | Async, but based on 'MVar', as we don't depend on @stm@.
data AsyncM a = Async
  { asyncThreadId :: {-# UNPACK #-} !ThreadId
  -- ^ Returns the 'ThreadId' of the thread running
  -- the given 'Async'.
  , _asyncMVar :: MVar (Either SomeException a)
  }

-- | Spawn an asynchronous action in a separate thread, and pass its
-- @Async@ handle to the supplied function.  When the function returns
-- or throws an exception, 'uninterruptibleCancel' is called on the @Async@.
--
-- > withAsync action inner = mask $ \restore -> do
-- >   a <- async (restore action)
-- >   restore (inner a) `finally` uninterruptibleCancel a
--
-- This is a useful variant of 'async' that ensures an @Async@ is
-- never left running unintentionally.
--
-- Note: a reference to the child thread is kept alive until the call
-- to `withAsync` returns, so nesting many `withAsync` calls requires
-- linear memory.
withAsync :: IO a -> (AsyncM a -> IO b) -> IO b
withAsync = inline withAsyncUsing forkIO

withAsyncNF :: NFData a => IO a -> (AsyncM a -> IO b) -> IO b
withAsyncNF m = inline withAsyncUsing forkIO (m >>= evaluateNF)
  where
    evaluateNF = evaluate . force

withAsyncUsing :: (IO () -> IO ThreadId) -> IO a -> (AsyncM a -> IO b) -> IO b
-- The bracket version works, but is slow.  We can do better by
-- hand-coding it:
withAsyncUsing doFork = \action inner -> do
  var <- newEmptyMVar
  mask $ \restore -> do
    t <- doFork $ try (restore action) >>= putMVar var
    let a = Async t var
    r <-
      restore (inner a) `catchAll` \e -> do
        uninterruptibleCancel a
        throwIO e
    uninterruptibleCancel a
    return r

-- | Wait for an asynchronous action to complete, and return its
-- value.  If the asynchronous action threw an exception, then the
-- exception is re-thrown by 'wait'.
--
-- > wait = atomically . waitSTM
{-# INLINE wait #-}
wait :: AsyncM a -> IO a
wait a = do
  res <- waitCatch a
  case res of
    Left (SomeException e) -> throwIO e
    Right x -> return x

-- | Wait for an asynchronous action to complete, and return either
-- @Left e@ if the action raised an exception @e@, or @Right a@ if it
-- returned a value @a@.
--
-- > waitCatch = atomically . waitCatchSTM
{-# INLINE waitCatch #-}
waitCatch :: AsyncM a -> IO (Either SomeException a)
waitCatch (Async _ var) = tryAgain (readMVar var)
  where
    -- See: https://github.com/simonmar/async/issues/14
    tryAgain f = f `catch` \BlockedIndefinitelyOnMVar -> f

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch

-- | Cancel an asynchronous action by throwing the @AsyncCancelled@
-- exception to it, and waiting for the `Async` thread to quit.
-- Has no effect if the 'Async' has already completed.
--
-- > cancel a = throwTo (asyncThreadId a) AsyncCancelled <* waitCatch a
--
-- Note that 'cancel' will not terminate until the thread the 'Async'
-- refers to has terminated. This means that 'cancel' will block for
-- as long said thread blocks when receiving an asynchronous exception.
--
-- For example, it could block if:
--
-- * It's executing a foreign call, and thus cannot receive the asynchronous
-- exception;
-- * It's executing some cleanup handler after having received the exception,
-- and the handler is blocking.
{-# INLINE cancel #-}
cancel :: AsyncM a -> IO ()
cancel a@(Async t _) = do
  throwTo t AsyncCancelled
  void (waitCatch a)

-- | The exception thrown by `cancel` to terminate a thread.
data AsyncCancelled = AsyncCancelled
  deriving
    ( Show
    , Eq
    , Typeable
    )

{- FOURMOLU_DISABLE -}
instance Exception AsyncCancelled where
#if MIN_VERSION_base(4,7,0)
  -- wraps in SomeAsyncException
  -- See https://github.com/ghc/ghc/commit/756a970eacbb6a19230ee3ba57e24999e4157b09
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException
#endif
{- FOURMOLU_ENABLE -}

-- | Cancel an asynchronous action
--
-- This is a variant of `cancel`, but it is not interruptible.
{-# INLINE uninterruptibleCancel #-}
uninterruptibleCancel :: AsyncM a -> IO ()
uninterruptibleCancel = uninterruptibleMask_ . cancel
