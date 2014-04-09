module UnitTests.Distribution.Simple.JobControl
    ( tests
    ) where

import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.IORef
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion, assertBool, assertEqual)

import Distribution.Simple.JobControl


tests :: [Test]
tests =
    [ testCase "withJobLimit" test_withJobLimit
    , testCase "withJobLimit with exceptions" test_withJobLimitExceptions
    ]


-- Convenicence.
atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef' ref (\x -> (f x, ()))



test_withJobLimit :: Assertion
test_withJobLimit = do

  let maxJobs = 4   :: Int
      numJobs = 100 :: Int

  -- How many threads are currently in our `withJobLimit` region.
  -- This is only precise enough because we `threadDelay` to enforce our
  -- ordering  of events, but that is sufficient for this test to check if
  -- `withJobLimit` does throttle the jobs or not.
  insideRef <- newIORef (0 :: Int)

  limit <- newJobLimit maxJobs

  mvars <- forM [1..numJobs] $ \_ -> do
    mvar <- newEmptyMVar -- to signal when job is finished

    -- Spawn new thread, inside limit to JobLimit
    _ <- forkIO $ withJobLimit limit $ do

      -- Increase running jobs count and check its sanity
      inside <- atomicModifyIORef' insideRef (\n -> (n+1, n+1))
      assertBool "running jobs exceeded job limit" $ inside <= maxJobs
      assertBool "running jobs became too small"   $ inside > 0

      threadDelay 10000 -- wait 10 ms for job's "work"

      -- Decrease running jobs count
      atomicModifyIORef_ insideRef pred
      putMVar mvar () -- signal job finished

    return mvar

  -- Wait for all jobs to finish
  forM_ mvars takeMVar


-- This test checks that a JobLimit doesn't get confused about the number of
-- running jobs when a job throws an exception (that the internal job
-- capacity does not change), and that a failed job does not impact other jobs
-- that are currently running or will be run in the future.
test_withJobLimitExceptions :: Assertion
test_withJobLimitExceptions = do

  let maxJobs = 4   :: Int
      numJobs = 100 :: Int

  -- How many exceptions have been raised across all jobs
  exceptionCounterRef <- newIORef (0 :: Int)

  -- When a job raises an exception and that propagates out of the
  -- `withJobLimit`, we simply count it.
  -- This is to test that a failed job has no impact on the other jobs.
  let excHandler e = (e :: SomeException) `seq`
                        atomicModifyIORef_ exceptionCounterRef (+ 1)

  insideRef <- newIORef (0 :: Int)

  limit <- newJobLimit maxJobs

  mvars <- forM [1..numJobs] $ \jobNo -> do
    mvar <- newEmptyMVar -- to signal when job is finished

    -- Spawn new thread, inside limit to JobLimit
    _ <- forkIO $ handle excHandler $ withJobLimit limit $ do

      -- Increase running jobs count and check its sanity
      inside <- atomicModifyIORef' insideRef (\n -> (n+1, n+1))
      assertBool "running jobs exceeded job limit" $ inside <= maxJobs
      assertBool "running jobs became too small"   $ inside > 0

      threadDelay 10000 -- wait 10 ms for job's "work"

      -- Make every 5th job throw an exception (but still count the job as
      -- finished)
      finally (when (jobNo `mod` 5 == 0) $ error "job fails")
              $ do
                  -- Decrease running jobs count
                  atomicModifyIORef_ insideRef pred
                  putMVar mvar () -- signal job finished

    return mvar

  -- Wait for all jobs to finish
  forM_ mvars takeMVar

  -- Assert that the exceptions we expect really come through
  exceptionCounter <- readIORef exceptionCounterRef
  assertEqual "did not get enough jobs with exceptions" 20 exceptionCounter
