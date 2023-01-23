module UnitTests.Distribution.Simple.Command
    ( tests
    ) where

import Distribution.Simple.Command
import qualified Distribution.Simple.Flag as Flag
import Distribution.Simple.Setup (optionVerbosity, testOptions, testCommand, splitArgs)
import qualified Distribution.Verbosity as Verbosity
import Distribution.Simple.InstallDirs (toPathTemplate)
import Test.Tasty
import Test.Tasty.HUnit

argumentTests :: [TestTree]
argumentTests =
  [ testCase "parses verbosity successfully" $ do
      let p = commandParseArgs cmdUI isGlobal ["-v2"]
      assertEqual "expected verbose" (Right verbose) $ evalParseWith Flag.NoFlag p
  , testCase "handles argument parse error gracefully" $ do
      let p = commandParseArgs cmdUI isGlobal ["-v=2"]
      assertEqual "expected error" (Left "errors") $ evalParseWith Flag.NoFlag p
  , testCase "handles test-options successfully" $ do
      let p = commandParseArgs testCommand isGlobal ["--test-options=-p 'find root'"]
      assertEqual "expected test options split" (Right $ fmap toPathTemplate ["-p", "find root"]) $
        (fmap testOptions (evalParseWith (commandDefaultFlags testCommand) p))
  , testCase "handles test-options successfully" $ do
      let p = commandParseArgs testCommand isGlobal ["--test-options=-p \"find root\""]
      assertEqual "expected test options split" (Right $ fmap toPathTemplate ["-p", "find root"]) $
        (fmap testOptions (evalParseWith (commandDefaultFlags testCommand) p))
  , testCase "handle quoted things sensibly in options" $
      assertEqual "expect options to be split" ["--foo=C:/Program Files/Bar/", "--baz"] $
        splitArgs "--foo=\"C:/Program Files/Bar/\" --baz"
  , testCase "handle quoted things sensibly in options" $
      assertEqual "expect options to be split" ["-DMSGSTR=\"foo bar\"", "--baz"] $
        splitArgs "\"-DMSGSTR=\\\"foo bar\\\"\" --baz"
  , testCase "handle quoted things sensibly in options" $
      assertEqual "expect options to be split" ["-p", "find root"] $
        splitArgs "-p 'find root'"
  ]
  where
    -- evaluate command parse result, to force possible exceptions in 'f'
    evalParseWith fs p = case p of
      CommandErrors _         -> Left "errors"
      CommandHelp _           -> Left "help"
      CommandList _           -> Left "list"
      CommandReadyToGo (f, _) -> Right $ f fs
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
