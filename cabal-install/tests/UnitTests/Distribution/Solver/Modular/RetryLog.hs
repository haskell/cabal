{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UnitTests.Distribution.Solver.Modular.RetryLog (
  tests
  ) where

import Distribution.Solver.Modular.Message
import Distribution.Solver.Modular.RetryLog
import Distribution.Solver.Types.Progress

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

type Log a = Progress a String String

tests :: [TestTree]
tests = [
    testCase "convert to and from RetryLog ending in failure" $
        let lg = Step 1 (Step 2 (Step 3 (Fail "Error")))
        in toProgress (fromProgress lg) @?= (lg :: Log Int)

  , testCase "convert to and from RetryLog ending in success" $
        let lg = Step 1 (Step 2 (Step 3 (Done "Result")))
        in toProgress (fromProgress lg) @?= (lg :: Log Int)

  , testCase "retry with failure" $
        let log1 = fromProgress $ Step 1 (Step 2 (Fail "Error 1"))
            log2 = fromProgress $ Step 3 (Step 4 (Fail "Error 2"))
        in toProgress (retry log1 (const log2))
           @?= (Step 1 (Step 2 (Step 3 (Step 4 (Fail "Error 2")))) :: Log Int)

  , testCase "retry with success" $
        let lg1 = fromProgress $ Step 1 (Step 2 (Done "Done"))
            lg2 = fromProgress $ Step 3 (Step 4 (Fail "Error"))
        in toProgress (retry lg1 (const lg2))
           @?= (Step 1 (Step 2 (Done "Done")) :: Log Int)

  , testCase "failWith" $
        toProgress (failWith 1 "Error") @?= (Step 1 (Fail "Error") :: Log Int)

  , testCase "succeedWith" $
        toProgress (succeedWith 1 "Result")
        @?= (Step 1 (Done "Result") :: Log Int)

  , testCase "continueWith" $
        let failure = Fail "Error"
        in toProgress (continueWith 1 $ fromProgress failure)
           @?= (Step 1 failure :: Log Int)

  , testCase "tryWith with failure" $
        let failure = Fail "Error"
            s = Step Success
        in toProgress (tryWith Success $ fromProgress (s (s failure)))
           @?= (s (Step Enter (s (s (Step Leave failure)))) :: Log Message)

  , testCase "tryWith with success" $
        let done = Done "Done"
            s = Step Success
        in toProgress (tryWith Success $ fromProgress (s (s done)))
           @?= (s (Step Enter (s (s done))) :: Log Message)
  ]

deriving instance (Eq step, Eq fail, Eq done) => Eq (Progress step fail done)

deriving instance (Show step, Show fail, Show done)
    => Show (Progress step fail done)

deriving instance Eq Message
deriving instance Show Message
