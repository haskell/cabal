{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Solver.Modular.RetryLog
    ( RetryLog
    , toProgress
    , fromProgress
    , retry
    , failWith
    , succeedWith
    , continueWith
    , tryWith
    ) where

import Distribution.Solver.Modular.Message
import Distribution.Solver.Types.Progress

-- | 'Progress' as a difference list that allows efficient appends at failures.
newtype RetryLog step fail done = RetryLog {
    unRetryLog :: (fail -> Progress step fail done)
               -> Progress step fail done
  }

-- | /O(1)/. Convert a 'RetryLog' to a 'Progress'.
toProgress :: RetryLog step fail done -> Progress step fail done
toProgress (RetryLog f) = f Fail

-- | /O(N)/. Convert a 'Progress' to a 'RetryLog'.
fromProgress :: forall step fail done .
                Progress step fail done
             -> RetryLog step fail done
fromProgress l = RetryLog $ \f ->
  let go :: Progress step fail done -> Progress step fail done
      go (Done d) = Done d
      go (Fail failure) = f failure
      go (Step m ms) = Step m (go ms)
  in go l

-- | /O(1)/. If the first log leads to failure, continue with the second.
retry :: RetryLog step fail done
      -> (fail -> RetryLog step fail done)
      -> RetryLog step fail done
retry (RetryLog f) g =
    RetryLog $ \extendLog -> f $ \failure -> unRetryLog (g failure) extendLog

-- | /O(1)/. Create a log with one message before a failure.
failWith :: step -> fail -> RetryLog step fail done
failWith m failure = RetryLog $ \f -> Step m (f failure)

-- | /O(1)/. Create a log with one message before a success.
succeedWith :: step -> done -> RetryLog step fail done
succeedWith m d = RetryLog $ const $ Step m (Done d)

-- | /O(1)/. Prepend a message to a log.
continueWith :: step
             -> RetryLog step fail done
             -> RetryLog step fail done
continueWith m (RetryLog f) = RetryLog $ Step m . f

-- | /O(1)/. Prepend the given message and 'Enter' to the log, and insert
-- 'Leave' before the failure if the log fails.
tryWith :: Message -> RetryLog Message fail done -> RetryLog Message fail done
tryWith m f =
  RetryLog $ Step m . Step Enter . unRetryLog (retry f (failWith Leave))
