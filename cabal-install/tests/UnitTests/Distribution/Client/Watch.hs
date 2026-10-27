module UnitTests.Distribution.Client.Watch
  ( tests
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.IORef
import Test.Tasty
import Test.Tasty.HUnit

import Distribution.Client.Watch
import Distribution.Verbosity
  ( Verbosity
  , defaultVerbosityHandles
  , mkVerbosity
  , silent
  )

testVerb :: Verbosity
testVerb = mkVerbosity defaultVerbosityHandles silent

tests :: [TestTree]
tests =
  [ testCase "watchLoopRunsCommandThenRerunsOnEvent" loopRunsThenReruns
  , testCase "watchLoopDrainsQueuedEvents" loopDrainsQueuedEvents
  , testCase "watchLoopSurvivesCommandFailure" loopSurvivesFailure
  , testCase "watchLoopStopsOnNothing" loopStopsOnNothing
  , testCase "watchEventRelevantMatchesWatchedName" relevantWatchedName
  , testCase "watchEventRelevantMatchesCreateExtension" relevantCreateExtension
  , testCase "watchEventRelevantRejectsOtherEvents" relevantRejectsOthers
  ]

-- Scripted event source: yields the queued paths in order, then Nothing.
scriptedSource :: [Maybe FilePath] -> IO (IO (Maybe FilePath))
scriptedSource events = do
  rest <- newIORef events
  pure $ do
    evs <- readIORef rest
    case evs of
      [] -> pure Nothing
      (e : es) -> do
        writeIORef rest es
        pure e

loopRunsThenReruns :: Assertion
loopRunsThenReruns = do
  next <- scriptedSource [Just "A.hs", Nothing]
  count <- newIORef (0 :: Int)
  watchLoop testVerb next $ atomicModifyIORef' count (\n -> (n + 1, ()))
  readIORef count >>= (@?= 2)

loopDrainsQueuedEvents :: Assertion
loopDrainsQueuedEvents = do
  -- Mimics the debounced adapter: queued events are already coalesced into
  -- one path per rerun by the time the loop sees them.
  events <- newIORef (["A.hs", "B.hs"] :: [FilePath])
  count <- newIORef (0 :: Int)
  let next = do
        evs <- readIORef events
        case evs of
          [] -> pure Nothing
          (e : es) -> do
            writeIORef events es
            pure (Just e)
  watchLoop testVerb next $ atomicModifyIORef' count (\n -> (n + 1, ()))
  readIORef count >>= (@?= 3)

loopSurvivesFailure :: Assertion
loopSurvivesFailure = do
  next <- scriptedSource [Just "A.hs", Just "B.hs", Nothing]
  count <- newIORef (0 :: Int)
  let failingCmd = do
        n <- atomicModifyIORef' count (\m -> let m' = m + 1 in (m', m'))
        when (n == 2) $ throwIO (userError "simulated build failure")
  -- The first re-run (second invocation) fails; watchLoop must catch it
  -- and continue on the next event.
  watchLoop testVerb next failingCmd
  readIORef count >>= (@?= 3)

loopStopsOnNothing :: Assertion
loopStopsOnNothing = do
  next <- scriptedSource []
  count <- newIORef (0 :: Int)
  watchLoop testVerb next $ atomicModifyIORef' count (\n -> (n + 1, ()))
  readIORef count >>= (@?= 1)

relevantWatchedName :: Assertion
relevantWatchedName = do
  assertBool "modified watched file" $
    watchEventRelevant ["A.hs", "p.cabal"] ("/proj/A.hs", False)
  assertBool "create of watched file" $
    watchEventRelevant ["A.hs"] ("/proj/A.hs", True)

relevantCreateExtension :: Assertion
relevantCreateExtension = do
  assertBool "create of new module" $
    watchEventRelevant ["B.hs"] ("/proj/New.hs", True)
  assertBool "create of new cabal file" $
    watchEventRelevant [] ("/proj/new.cabal", True)

relevantRejectsOthers :: Assertion
relevantRejectsOthers = do
  assertBool "unrelated modified file" $
    not (watchEventRelevant ["A.hs"] ("/proj/README.md", False))
  assertBool "unrelated created file" $
    not (watchEventRelevant ["A.hs"] ("/proj/notes.txt", True))
