module Control.Concurrent.Async (
    Async,
    async
  ) where


-- -----------------------------------------------------------------------------
-- STM Async API


-- | An asynchronous action spawned by 'async' or 'withAsync'.
-- Asynchronous actions are executed in a separate thread, and
-- operations are provided for waiting for asynchronous actions to
-- complete and obtaining their results (see e.g. 'wait').
--
data Async a = Async

-- | Spawn an asynchronous action in a separate thread.
--
-- Like for 'forkIO', the action may be left running unintentinally
-- (see module-level documentation for details).
--
-- __Use 'withAsync' style functions wherever you can instead!__
async :: IO a -> IO (Async a)
async = undefined
