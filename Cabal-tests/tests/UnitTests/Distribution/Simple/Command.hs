module UnitTests.Distribution.Simple.Command
    ( tests
    ) where

import Distribution.Simple.Command
import qualified Distribution.Simple.Flag as Flag
import Distribution.Simple.Setup (optionVerbosity)
import qualified Distribution.Simple.Setup as Setup
import Distribution.Simple.Program
import Distribution.Simple.Program.Types (simpleConfiguredProgram)
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
      , commandUsage = (++ " cmd -v[N]")
      , commandDescription = Nothing
      , commandNotes = Nothing
      , commandDefaultFlags = Flag.NoFlag
      , commandOptions = const [ optField ]
      }
    optField = optionVerbosity id const

tests :: [TestTree]
tests =
  [ testGroup "option argument tests" argumentTests
  , testCase "build invocation defaults preserve argument order and overrides" $
      checkInvocationDefaults (Setup.buildCommand defaultProgramDb) Setup.buildProgramPrependArgs
  , testCase "Haddock invocation defaults preserve argument order and overrides" $
      checkInvocationDefaults Setup.haddockCommand Setup.haddockProgramPrependArgs
  , testCase "repl invocation defaults preserve argument order and overrides" $
      checkInvocationDefaults (Setup.replCommand defaultProgramDb) Setup.replProgramPrependArgs
  ]

checkInvocationDefaults :: CommandUI flags -> (flags -> [(String, [String])]) -> Assertion
checkInvocationDefaults command getArgs = do
    let options = ["--ghc-prepend-option=-haddock", "--ghc-prepend-option=-fwrite-ide-info"]
        explicitArgs = ["-fno-write-ide-info", "-fplugin-opt=Example:once"]
        ghc = (simpleConfiguredProgram "ghc" (UserSpecified "ghc"))
            { programOverrideArgs = explicitArgs }
        configured = updateProgram ghc defaultProgramDb
    case commandParseArgs command False options of
      CommandReadyToGo (setFlags, []) -> do
        let flags = setFlags (commandDefaultFlags command)
            defaults = getArgs flags
            invocation = prependProgramArgs defaults configured
        assertEqual "defaults survive CLI serialization" options (commandShowOptions command flags `filterDefaults` options)
        case lookupProgram ghcProgram invocation of
          Just prog -> assertEqual "defaults precede user arguments without duplication"
            (["-haddock", "-fwrite-ide-info"] ++ explicitArgs) (programOverrideArgs prog)
          Nothing -> assertFailure "lost configured GHC"
        assertEqual "persisted arguments unchanged" (Just explicitArgs)
            (programOverrideArgs <$> lookupProgram ghcProgram configured)
      _ -> assertFailure "could not parse invocation defaults"
  where
    filterDefaults actual expected = filter (`elem` expected) actual
