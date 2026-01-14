{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Exception type like 'ExitCode' but with more information
-- than just integer.
module Test.Cabal.TestCode
  ( -- * TestCode
    TestCode (..)
  , FlakyStatus (..)
  , IssueID (..)
  , displayTestCode
  , isTestCodeSkip
  , isTestCodeFlaky
  , isTestCodeUnexpectedSuccess
  ) where

import Control.Exception (Exception (..))

-------------------------------------------------------------------------------
-- TestCode
-------------------------------------------------------------------------------

data TestCode
  = TestCodeOk
  | TestCodeSkip String
  | TestCodeKnownFail IssueID
  | TestCodeUnexpectedOk IssueID
  | TestCodeFail
  | TestCodeFlakyFailed IssueID
  | TestCodeFlakyPassed IssueID
  deriving (Eq, Show, Read)

instance Exception TestCode where
  displayException = displayTestCode

displayTestCode :: TestCode -> String
displayTestCode TestCodeOk = "OK"
displayTestCode (TestCodeSkip msg) = "SKIP " ++ msg
displayTestCode (TestCodeKnownFail t) = "OK (known failure, see #" <> show t <> ")"
displayTestCode (TestCodeUnexpectedOk t) = "FAIL (unexpected success, see #" <> show t <> ")"
displayTestCode TestCodeFail = "FAIL"
displayTestCode (TestCodeFlakyFailed t) = "FLAKY (FAIL, see #" <> show t <> ")"
displayTestCode (TestCodeFlakyPassed t) = "FLAKY (OK, see #" <> show t <> ")"

isTestCodeSkip :: TestCode -> Bool
isTestCodeSkip (TestCodeSkip _) = True
isTestCodeSkip _ = False

type TestPassed = Bool

newtype IssueID = IssueID Int
  deriving newtype (Eq, Num, Show, Read)

data FlakyStatus
  = NotFlaky
  | Flaky TestPassed IssueID

isTestCodeFlaky :: TestCode -> FlakyStatus
isTestCodeFlaky (TestCodeFlakyPassed t) = Flaky True t
isTestCodeFlaky (TestCodeFlakyFailed t) = Flaky False t
isTestCodeFlaky _ = NotFlaky

isTestCodeUnexpectedSuccess :: TestCode -> Maybe IssueID
isTestCodeUnexpectedSuccess (TestCodeUnexpectedOk t) = Just t
isTestCodeUnexpectedSuccess _ = Nothing
