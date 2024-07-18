{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Exception type like 'ExitCode' but with more information
-- than just integer.
module Test.Cabal.TestCode (
    -- * TestCode
    TestCode (..),
    displayTestCode,
    isTestCodeSkip,
) where

import Control.Exception (Exception (..))
import Data.Typeable     (Typeable)

-------------------------------------------------------------------------------
-- TestCode
-------------------------------------------------------------------------------

data TestCode
    = TestCodeOk
    | TestCodeSkip String
    | TestCodeKnownFail Int
    | TestCodeUnexpectedOk
    | TestCodeFail
  deriving (Eq, Show, Read, Typeable)

instance Exception TestCode
#if MIN_VERSION_base(4,8,0)
  where
    displayException = displayTestCode
#endif

displayTestCode :: TestCode -> String
displayTestCode TestCodeOk            = "OK"
displayTestCode (TestCodeSkip msg)    = "SKIP " ++ msg
displayTestCode (TestCodeKnownFail t) = "OK (known failure, see #" <> show t <> ")"
displayTestCode TestCodeUnexpectedOk  = "FAIL (unexpected success)"
displayTestCode TestCodeFail          = "FAIL"

isTestCodeSkip :: TestCode -> Bool
isTestCodeSkip (TestCodeSkip _) = True
isTestCodeSkip _                = False
