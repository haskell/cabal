-- | Running a dependency graph of jobs concurrently.
module Distribution.Client.Buck2.Schedule
  ( runDependencyGraph
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
  ( atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , retry
  , writeTVar
  )

import Distribution.Simple.Utils (ordNub)

-- | @runDependencyGraph n deps process@ runs @process k@ once for every key
-- @k@ of @deps@, on up to @n@ threads, but only after @process@ has finished
-- for each of the keys @deps@ lists for @k@. Dependencies that aren't keys of
-- @deps@ are ignored. Returns when every job has finished; if one throws, the
-- rest are cancelled and the exception is rethrown. The graph must be
-- acyclic.
--
-- Jobs are scheduled by their direct dependencies alone, so a job runs as soon
-- as the jobs it depends on are done, whatever else is still running - not
-- wave by wave of a topological sort.
--
-- This is a small hand-rolled STM worker pool rather than something built on
-- "Control.Concurrent.Stream", whose termination protocol assumes a single
-- producer that knows the whole worklist up front. With jobs enqueued by the
-- workers as they unblock their dependents, its producer can decide that all
-- work has been submitted, and stop every worker, while a worker has decided a
-- dependent is ready but not yet queued it; that dependent is then silently
-- never run. Here, instead, 'finishJob' marks a job done and queues the newly
-- ready jobs in one STM transaction, and a worker only gives up once the
-- count of finished jobs reaches the total, at which point nothing more can be
-- queued. An empty queue with work outstanding makes a worker 'retry'.
runDependencyGraph :: Ord k => Int -> Map k [k] -> (k -> IO ()) -> IO ()
runDependencyGraph numWorkers deps process = do
  let jobs = Map.keysSet deps
      dependenciesOf = Map.map (ordNub . filter (`Set.member` jobs)) deps
      dependents = Map.fromListWith (++) [(d, [k]) | (k, ds) <- Map.toList dependenciesOf, d <- ds]
      totalJobs = Map.size deps

  remainingVar <- newTVarIO (Map.map length dependenciesOf)
  readyVar <- newTVarIO [k | (k, []) <- Map.toList dependenciesOf]
  finishedVar <- newTVarIO (0 :: Int)

  let finishJob k = do
        remaining <- readTVar remainingVar
        let (remaining', newlyReady) =
              foldl'
                ( \(rs, ready) d ->
                    let n = Map.findWithDefault 0 d rs - 1
                     in (Map.insert d n rs, if n == 0 then d : ready else ready)
                )
                (remaining, [])
                (Map.findWithDefault [] k dependents)
        writeTVar remainingVar remaining'
        modifyTVar' readyVar (newlyReady ++)
        modifyTVar' finishedVar (+ 1)

      nextJob = do
        ready <- readTVar readyVar
        case ready of
          (k : rest) -> writeTVar readyVar rest >> return (Just k)
          [] -> do
            finished <- readTVar finishedVar
            if finished == totalJobs then return Nothing else retry

      worker = do
        mk <- atomically nextJob
        for_ mk $ \k -> do
          process k
          atomically (finishJob k)
          worker

  Async.replicateConcurrently_ numWorkers worker
