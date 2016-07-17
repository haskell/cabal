{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Command.Test
-- Copyright   :  Thomas Tuegel 2016
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module defines command-line interface to the @test@ action.

module Distribution.Simple.Command.Test where

import GHC.Generics ( Generic )
import qualified Text.PrettyPrint as Disp

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.Semigroup
import Distribution.Flag
import Distribution.ReadE
import Distribution.Simple.Command
import Distribution.Simple.InstallDirs
import Distribution.Simple.Utils
import Distribution.Text
import Distribution.Verbosity

import Data.Char ( isAlpha )


data TestConfig a = TestFlags { testDistPref    :: a FilePath
                              , testVerbosity   :: a Verbosity
                              , testHumanLog    :: a PathTemplate
                              , testMachineLog  :: a PathTemplate
                              , testShowDetails :: a TestShowDetails
                              , testKeepTix     :: a Bool
                              , testOptions     :: [PathTemplate]
                                -- TODO: think about if/how options are
                                -- passed to test exes
                              }
  deriving (Generic)

type TestFlags = TestConfig Flag

instance Monoid (TestConfig Flag) where
    mempty = gmempty
    mappend = (<>)

instance Semigroup (TestConfig Flag) where
    (<>) = gmappend

finalizeTestFlags :: TestFlags -> TestConfig Final
finalizeTestFlags flags =
    TestFlags { testDistPref = finalizeDistPref testDistPref flags
              , testVerbosity = finalizeVerbosity testVerbosity flags
              , testHumanLog = finalize testHumanLog defaultHumanLog
              , testMachineLog = finalize testMachineLog defaultMachineLog
              , testShowDetails = finalize testShowDetails Failures
              , testKeepTix = finalize testKeepTix False
              , testOptions = testOptions flags
              }
  where
    finalize get def = Final (fromFlagOrDefault def (get flags))
    defaultHumanLog = toPathTemplate "$pkgid-$test-suite.log"
    defaultMachineLog = toPathTemplate "$pkgid.log"

testConfigToFlags :: TestConfig Final -> TestFlags
testConfigToFlags config =
    TestFlags { testDistPref = unfinalize testDistPref
              , testVerbosity = unfinalize testVerbosity
              , testHumanLog = unfinalize testHumanLog
              , testMachineLog = unfinalize testMachineLog
              , testShowDetails = unfinalize testShowDetails
              , testKeepTix = unfinalize testKeepTix
              , testOptions = testOptions config
              }
  where
    unfinalize get = (toFlag . fromFinal) (get config)

{-# DEPRECATED emptyTestFlags "Use mempty instead." #-}
emptyTestFlags :: TestFlags
emptyTestFlags = mempty

{-# DEPRECATED defaultTestFlags "Use finalizeTestFlags instead." #-}
defaultTestFlags :: TestFlags
defaultTestFlags = testConfigToFlags (finalizeTestFlags emptyTestFlags)

data TestShowDetails = Never | Failures | Always | Streaming | Direct
    deriving (Eq, Ord, Enum, Bounded, Show)

knownTestShowDetails :: [TestShowDetails]
knownTestShowDetails = [minBound..maxBound]

instance Text TestShowDetails where
    disp  = Disp.text . lowercase . show

    parse = maybe Parse.pfail return . classify =<< ident
      where
        ident        = Parse.munch1 (\c -> isAlpha c || c == '_' || c == '-')
        classify str = lookup (lowercase str) enumMap
        enumMap     :: [(String, TestShowDetails)]
        enumMap      = [ (display x, x)
                       | x <- knownTestShowDetails ]

--TODO: do we need this instance?
instance Monoid TestShowDetails where
    mempty = Never
    mappend = (<>)

instance Semigroup TestShowDetails where
    a <> b = if a < b then b else a

testCommand :: CommandUI TestFlags
testCommand = CommandUI
  { commandName         = "test"
  , commandSynopsis     =
      "Run all/specific tests in the test suite."
  , commandDescription  = Just $ \pname -> wrapText $
         "If necessary (re)configures with `--enable-tests` flag and builds"
      ++ " the test suite.\n"
      ++ "\n"
      ++ "Remember that the tests' dependencies must be installed if there"
      ++ " are additional ones; e.g. with `" ++ pname
      ++ " install --only-dependencies --enable-tests`.\n"
      ++ "\n"
      ++ "By defining UserHooks in a custom Setup.hs, the package can"
      ++ " define actions to be executed before and after running tests.\n"
  , commandNotes        = Nothing
  , commandUsage        = usageAlternatives "test"
      [ "[FLAGS]"
      , "TESTCOMPONENTS [FLAGS]"
      ]
  , commandDefaultFlags = defaultTestFlags
  , commandOptions = \showOrParseArgs ->
      [ optionVerbosity testVerbosity (\v flags -> flags { testVerbosity = v })
      , optionDistPref
            testDistPref (\d flags -> flags { testDistPref = d })
            showOrParseArgs
      , option [] ["log"]
            ("Log all test suite results to file (name template can use "
            ++ "$pkgid, $compiler, $os, $arch, $test-suite, $result)")
            testHumanLog (\v flags -> flags { testHumanLog = v })
            (reqArg' "TEMPLATE"
                (toFlag . toPathTemplate)
                (flagToList . fmap fromPathTemplate))
      , option [] ["machine-log"]
            ("Produce a machine-readable log file (name template can use "
            ++ "$pkgid, $compiler, $os, $arch, $result)")
            testMachineLog (\v flags -> flags { testMachineLog = v })
            (reqArg' "TEMPLATE"
                (toFlag . toPathTemplate)
                (flagToList . fmap fromPathTemplate))
      , option [] ["show-details"]
            ("'always': always show results of individual test cases. "
             ++ "'never': never show results of individual test cases. "
             ++ "'failures': show results of failing test cases. "
             ++ "'streaming': show results of test cases in real time."
             ++ "'direct': send results of test cases in real time; no log file.")
            testShowDetails (\v flags -> flags { testShowDetails = v })
            (reqArg "FILTER"
                (readP_to_E (\_ -> "--show-details flag expects one of "
                              ++ intercalate ", "
                                   (map display knownTestShowDetails))
                            (fmap toFlag parse))
                (flagToList . fmap display))
      , option [] ["keep-tix-files"]
            "keep .tix files for HPC between test runs"
            testKeepTix (\v flags -> flags { testKeepTix = v})
            trueArg
      , option [] ["test-options"]
            ("give extra options to test executables "
             ++ "(name templates can use $pkgid, $compiler, "
             ++ "$os, $arch, $test-suite)")
            testOptions (\v flags -> flags { testOptions = v })
            (reqArg' "TEMPLATES" (map toPathTemplate . splitArgs)
                (const []))
      , option [] ["test-option"]
            ("give extra option to test executables "
             ++ "(no need to quote options containing spaces, "
             ++ "name template can use $pkgid, $compiler, "
             ++ "$os, $arch, $test-suite)")
            testOptions (\v flags -> flags { testOptions = v })
            (reqArg' "TEMPLATE" (\x -> [toPathTemplate x])
                (map fromPathTemplate))
      ]
  }
