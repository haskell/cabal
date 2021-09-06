module UnitTests.Distribution.Simple.Command
    ( tests
    ) where

import Distribution.Simple.Command
import qualified Distribution.Simple.Flag as Flag
import Distribution.Simple.Setup (optionVerbosity)
import qualified Distribution.Verbosity as Verbosity
import Test.Tasty
import Test.Tasty.HUnit

argumentTests :: [TestTree]
argumentTests =
  [ testCase "parses verbosity successfully" $ do
      let p = commandParseArgs cmdUI isGlobal ["-v2"]
      assertEqual "expected verbose" (Right verbose) $ evalParse p
  , testCase "handles argument parse error gracefully" $ do
      let p = commandParseArgs cmdUI isGlobal ["-v=2"]
      assertEqual "expected error" (Left "errors") $ evalParse p
  ]
  where
    -- evaluate command parse result, to force possible exceptions in 'f'
    evalParse p = case p of
      CommandErrors _         -> Left "errors"
      CommandHelp _           -> Left "help"
      CommandList _           -> Left "list"
      CommandReadyToGo (f, _) -> Right $ f Flag.NoFlag
    verbose = Flag.Flag Verbosity.verbose
    isGlobal = True
    cmdUI = CommandUI
      { commandName = "cmd"
      , commandSynopsis = "the command"
      , commandUsage = \name -> name ++ " cmd -v[N]"
      , commandDescription = Nothing
      , commandNotes = Nothing
      , commandDefaultFlags = Flag.NoFlag
      , commandOptions = const [ optField ]
      }
    optField = optionVerbosity id const
        
tests :: [TestTree]
tests =
  [ testGroup "option argument tests" argumentTests
  ]
