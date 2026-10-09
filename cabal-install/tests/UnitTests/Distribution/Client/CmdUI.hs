{-# LANGUAGE ScopedTypeVariables #-}

-- | Differential tests for "Distribution.Client.Cmd.UI": the
-- optparse-applicative parser must agree with "Distribution.Simple.Command",
-- the parser it replaces, on every command line generated from a command's
-- own option definitions.
module UnitTests.Distribution.Client.CmdUI (tests) where

import Distribution.Client.Cmd.UI (parseCommand)
import Distribution.ReadE (runReadE)
import Distribution.Simple.Command
  ( CommandParse (..)
  , CommandUI (..)
  , OptDescr (..)
  , OptionField (..)
  , ShowOrParseArgs (..)
  , commandParseArgs
  , commandShowOptions
  )

import qualified Distribution.Client.CmdBench as CmdBench
import qualified Distribution.Client.CmdBuild as CmdBuild
import qualified Distribution.Client.CmdClean as CmdClean
import qualified Distribution.Client.CmdConfigure as CmdConfigure
import qualified Distribution.Client.CmdExec as CmdExec
import qualified Distribution.Client.CmdFreeze as CmdFreeze
import qualified Distribution.Client.CmdGenBounds as CmdGenBounds
import qualified Distribution.Client.CmdHaddock as CmdHaddock
import qualified Distribution.Client.CmdHaddockProject as CmdHaddockProject
import qualified Distribution.Client.CmdInstall as CmdInstall
import qualified Distribution.Client.CmdOutdated as CmdOutdated
import qualified Distribution.Client.CmdRepl as CmdRepl
import qualified Distribution.Client.CmdRun as CmdRun
import qualified Distribution.Client.CmdSdist as CmdSdist
import qualified Distribution.Client.CmdTarget as CmdTarget
import qualified Distribution.Client.CmdTest as CmdTest
import qualified Distribution.Client.CmdUpdate as CmdUpdate

import Data.Either (isRight)
import Data.List (inits, isPrefixOf, nub)
import Data.Maybe (listToMaybe)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testGroup "optparse-applicative agrees with Distribution.Simple.Command" (map agreementTest commands)
  , testGroup "every option has a sample value" (map coverageTest commands)
  ]

-- | A command whose flags type is hidden, so the commands can be listed.
data SomeCommand = forall flags. SomeCommand (CommandUI flags)

commands :: [SomeCommand]
commands =
  [ SomeCommand CmdConfigure.configureCommand
  , SomeCommand CmdUpdate.updateCommand
  , SomeCommand CmdBuild.buildCommand
  , SomeCommand CmdRepl.replCommand
  , SomeCommand CmdFreeze.freezeCommand
  , SomeCommand CmdHaddock.haddockCommand
  , SomeCommand CmdHaddockProject.haddockProjectCommand
  , SomeCommand CmdInstall.installCommand
  , SomeCommand CmdRun.runCommand
  , SomeCommand CmdTest.testCommand
  , SomeCommand CmdBench.benchCommand
  , SomeCommand CmdExec.execCommand
  , SomeCommand CmdClean.cleanCommand
  , SomeCommand CmdSdist.sdistCommand
  , SomeCommand CmdTarget.targetCommand
  , SomeCommand CmdGenBounds.genBoundsCommand
  , SomeCommand CmdOutdated.outdatedCommand
  ]

-- | What a parse produced, in a form that can be compared: the options
-- rendered back to their command-line spelling by 'commandShowOptions',
-- and the targets.
data Outcome = Ready [String] [String] | Rejected
  deriving (Eq, Show)

legacyOutcome :: CommandUI flags -> [String] -> Outcome
legacyOutcome command args =
  case commandParseArgs command False args of
    CommandReadyToGo (setFlags, targets) ->
      Ready (commandShowOptions command (setFlags (commandDefaultFlags command))) targets
    _ -> Rejected

optparseOutcome :: CommandUI flags -> [String] -> Outcome
optparseOutcome command args =
  case parseCommand command (,) (commandName command) args of
    CommandReadyToGo (flags, targets) -> Ready (commandShowOptions command flags) targets
    _ -> Rejected

agreementTest :: SomeCommand -> TestTree
agreementTest (SomeCommand command) =
  testProperty (commandName command) $
    forAllShrink (genArgs command) shrinkArgs $ \args ->
      legacyOutcome command args === optparseOutcome command args

-- | Every option with an argument needs at least one sample value its
-- reader accepts, or the agreement test would never exercise it.
coverageTest :: SomeCommand -> TestTree
coverageTest (SomeCommand command) =
  testCase (commandName command) $
    assertEqual "options without an accepted sample value" [] uncovered
  where
    uncovered =
      [ name
      | OptionField name descrs <- commandOptions command ParseArgs
      , descr <- descrs
      , Just reader <- [readerOf descr]
      , null (acceptedValues reader)
      ]
    readerOf (ReqArg _ _ _ reader _) = Just (fmap (const ()) . runReadE reader)
    readerOf (OptArg _ _ _ reader _ _) = Just (fmap (const ()) . runReadE reader)
    readerOf _ = Nothing

-- | Candidate argument values; each option uses the ones its reader accepts.
-- This is the smallest set that gives every option with an argument, across
-- all the commands, at least one accepted value. Each entry names the options
-- that accept no other value in the set.
sampleValues :: [String]
sampleValues =
  [ "1" -- --verbose, --jobs, --max-backjumps, --cabal-lib-version
  , "always" -- --test-show-details, --overwrite-policy, --write-ghc-environment-files
  , "foo ==1.0" -- --constraint
  , "HEAD" -- --index-state
  , "Sig=foo-1.0:Impl" -- --instantiate-with
  , "copy" -- --install-method
  , "latest" -- --prefer-version
  , "legacy" -- --project-file-parser
  , "modular" -- --solver
  , "none" -- --remote-build-reporting, --reject-unconstrained-dependencies
  ]

acceptedValues :: (String -> Either e a) -> [String]
acceptedValues reader = filter (isRight . reader) sampleValues

-- | The command-line spellings of each option, from its definition: every
-- long and short name, with a sample value where one is taken, plus the
-- shortest unambiguous abbreviation of each long name.
optionSpellings :: CommandUI flags -> [[String]]
optionSpellings command = concatMap spellings descrs ++ abbreviations
  where
    descrs = [descr | OptionField _ ds <- commandOptions command ParseArgs, descr <- ds]

    spellings (ReqArg _ (shorts, longs) _ reader _) =
      [ form
      | value <- take 1 (acceptedValues (runReadE reader))
      , form <-
          [["--" ++ long ++ "=" ++ value] | long <- longs]
            ++ [["--" ++ long, value] | long <- longs]
            ++ [["-" ++ [short], value] | short <- shorts]
            ++ [["-" ++ [short] ++ value] | short <- shorts]
      ]
    spellings (OptArg _ (shorts, longs) _ reader _ _) =
      [["--" ++ long] | long <- longs]
        ++ [["-" ++ [short]] | short <- shorts]
        ++ [ form
           | value <- take 1 (acceptedValues (runReadE reader))
           , form <-
              [["--" ++ long ++ "=" ++ value] | long <- longs]
                ++ [["-" ++ [short] ++ value] | short <- shorts]
           ]
    spellings (ChoiceOpt choices) =
      [ form
      | (_, (shorts, longs), _, _) <- choices
      , form <- [["--" ++ long] | long <- longs] ++ [["-" ++ [short]] | short <- shorts]
      ]
    spellings (BoolOpt _ trueFlags falseFlags _ _) =
      [ form
      | (shorts, longs) <- [trueFlags, falseFlags]
      , form <- [["--" ++ long] | long <- longs] ++ [["-" ++ [short]] | short <- shorts]
      ]

    -- The names the legacy parser disambiguates against include the common
    -- --help and --list-options, which the generated lines never use.
    allLongNames = nub $ ["help", "list-options"] ++ concatMap longNamesOf descrs
    longNamesOf (ReqArg _ (_, longs) _ _ _) = longs
    longNamesOf (OptArg _ (_, longs) _ _ _ _) = longs
    longNamesOf (ChoiceOpt choices) = concat [longs | (_, (_, longs), _, _) <- choices]
    longNamesOf (BoolOpt _ (_, trueLongs) (_, falseLongs) _ _) = trueLongs ++ falseLongs

    abbreviations =
      [ ["--" ++ prefix ++ suffix]
      | descr <- descrs
      , long <- longNamesOf descr
      , Just prefix <- [shortestUniquePrefix long]
      , suffix <- case descr of
          ReqArg _ _ _ reader _ -> ['=' : value | value <- take 1 (acceptedValues (runReadE reader))]
          _ -> [""]
      ]
    shortestUniquePrefix long =
      listToMaybe
        [ prefix
        | prefix <- drop 1 (inits long)
        , prefix /= long
        , [long] == filter (prefix `isPrefixOf`) allLongNames
        ]

sampleTargets :: [String]
sampleTargets = ["pkg", "lib:foo", "exe:bar", "./dir", "all"]

genArgs :: CommandUI flags -> Gen [String]
genArgs command = do
  items <- listOf item
  trailing <- frequency [(3, pure []), (1, ("--" :) <$> listOf (elements ("--bogus" : "-x" : sampleTargets)))]
  pure (concat items ++ trailing)
  where
    spellings = optionSpellings command
    item =
      frequency $
        [(6, elements spellings) | not (null spellings)]
          ++ [(3, (: []) <$> elements sampleTargets), (1, pure ["--bogus"])]

shrinkArgs :: [String] -> [[String]]
shrinkArgs = shrinkList (const [])
