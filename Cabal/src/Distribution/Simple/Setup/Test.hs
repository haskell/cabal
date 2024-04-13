{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Test
-- Copyright   :  Isaac Jones 2003-2004
--                Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Definition of the testing command-line options.
-- See: @Distribution.Simple.Setup@
module Distribution.Simple.Setup.Test
  ( TestFlags
      ( TestCommonFlags
      , testVerbosity
      , testDistPref
      , testCabalFilePath
      , testWorkingDir
      , testTargets
      , ..
      )
  , emptyTestFlags
  , defaultTestFlags
  , testCommand
  , TestShowDetails (..)
  , testOptions'
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import Distribution.Parsec
import Distribution.Pretty
import Distribution.ReadE
import Distribution.Simple.Command hiding (boolOpt, boolOpt')
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs
import Distribution.Simple.Setup.Common
import Distribution.Simple.Utils
import Distribution.Utils.Path
import Distribution.Verbosity

import qualified Text.PrettyPrint as Disp

-- ------------------------------------------------------------

-- * Test flags

-- ------------------------------------------------------------

data TestShowDetails = Never | Failures | Always | Streaming | Direct
  deriving (Eq, Ord, Enum, Bounded, Generic, Show, Typeable)

instance Binary TestShowDetails
instance Structured TestShowDetails

knownTestShowDetails :: [TestShowDetails]
knownTestShowDetails = [minBound .. maxBound]

instance Pretty TestShowDetails where
  pretty = Disp.text . lowercase . show

instance Parsec TestShowDetails where
  parsec = maybe (fail "invalid TestShowDetails") return . classify =<< ident
    where
      ident = P.munch1 (\c -> isAlpha c || c == '_' || c == '-')
      classify str = lookup (lowercase str) enumMap
      enumMap :: [(String, TestShowDetails)]
      enumMap =
        [ (prettyShow x, x)
        | x <- knownTestShowDetails
        ]

-- TODO: do we need this instance?
instance Monoid TestShowDetails where
  mempty = Never
  mappend = (<>)

instance Semigroup TestShowDetails where
  a <> b = if a < b then b else a

data TestFlags = TestFlags
  { testCommonFlags :: !CommonSetupFlags
  , testHumanLog :: Flag PathTemplate
  , testMachineLog :: Flag PathTemplate
  , testShowDetails :: Flag TestShowDetails
  , testKeepTix :: Flag Bool
  , testWrapper :: Flag FilePath
  , testFailWhenNoTestSuites :: Flag Bool
  , -- TODO: think about if/how options are passed to test exes
    testOptions :: [PathTemplate]
  }
  deriving (Show, Generic, Typeable)

pattern TestCommonFlags
  :: Flag Verbosity
  -> Flag (SymbolicPath Pkg (Dir Dist))
  -> Flag (SymbolicPath CWD (Dir Pkg))
  -> Flag (SymbolicPath Pkg File)
  -> [String]
  -> TestFlags
pattern TestCommonFlags
  { testVerbosity
  , testDistPref
  , testWorkingDir
  , testCabalFilePath
  , testTargets
  } <-
  ( testCommonFlags ->
      CommonSetupFlags
        { setupVerbosity = testVerbosity
        , setupDistPref = testDistPref
        , setupWorkingDir = testWorkingDir
        , setupCabalFilePath = testCabalFilePath
        , setupTargets = testTargets
        }
    )

instance Binary TestFlags
instance Structured TestFlags

defaultTestFlags :: TestFlags
defaultTestFlags =
  TestFlags
    { testCommonFlags = defaultCommonSetupFlags
    , testHumanLog = toFlag $ toPathTemplate $ "$pkgid-$test-suite.log"
    , testMachineLog = toFlag $ toPathTemplate $ "$pkgid.log"
    , testShowDetails = toFlag Direct
    , testKeepTix = toFlag False
    , testWrapper = NoFlag
    , testFailWhenNoTestSuites = toFlag False
    , testOptions = []
    }

testCommand :: CommandUI TestFlags
testCommand =
  CommandUI
    { commandName = "test"
    , commandSynopsis =
        "Run all/specific tests in the test suite."
    , commandDescription = Just $ \_pname ->
        wrapText $
          testOrBenchmarkHelpText "test"
    , commandNotes = Nothing
    , commandUsage =
        usageAlternatives
          "test"
          [ "[FLAGS]"
          , "TESTCOMPONENTS [FLAGS]"
          ]
    , commandDefaultFlags = defaultTestFlags
    , commandOptions = testOptions'
    }

testOptions' :: ShowOrParseArgs -> [OptionField TestFlags]
testOptions' showOrParseArgs =
  withCommonSetupOptions
    testCommonFlags
    (\c f -> f{testCommonFlags = c})
    showOrParseArgs
    [ option
        []
        ["log"]
        ( "Log all test suite results to file (name template can use "
            ++ "$pkgid, $compiler, $os, $arch, $test-suite, $result)"
        )
        testHumanLog
        (\v flags -> flags{testHumanLog = v})
        ( reqArg'
            "TEMPLATE"
            (toFlag . toPathTemplate)
            (flagToList . fmap fromPathTemplate)
        )
    , option
        []
        ["machine-log"]
        ( "Produce a machine-readable log file (name template can use "
            ++ "$pkgid, $compiler, $os, $arch, $result)"
        )
        testMachineLog
        (\v flags -> flags{testMachineLog = v})
        ( reqArg'
            "TEMPLATE"
            (toFlag . toPathTemplate)
            (flagToList . fmap fromPathTemplate)
        )
    , option
        []
        ["show-details"]
        ( "'always': always show results of individual test cases. "
            ++ "'never': never show results of individual test cases. "
            ++ "'failures': show results of failing test cases. "
            ++ "'streaming': show results of test cases in real time."
            ++ "'direct': send results of test cases in real time; no log file."
        )
        testShowDetails
        (\v flags -> flags{testShowDetails = v})
        ( reqArg
            "FILTER"
            ( parsecToReadE
                ( \_ ->
                    "--show-details flag expects one of "
                      ++ intercalate
                        ", "
                        (map prettyShow knownTestShowDetails)
                )
                (fmap toFlag parsec)
            )
            (flagToList . fmap prettyShow)
        )
    , option
        []
        ["keep-tix-files"]
        "keep .tix files for HPC between test runs"
        testKeepTix
        (\v flags -> flags{testKeepTix = v})
        trueArg
    , option
        []
        ["test-wrapper"]
        "Run test through a wrapper."
        testWrapper
        (\v flags -> flags{testWrapper = v})
        ( reqArg'
            "FILE"
            (toFlag :: FilePath -> Flag FilePath)
            (flagToList :: Flag FilePath -> [FilePath])
        )
    , option
        []
        ["fail-when-no-test-suites"]
        ("Exit with failure when no test suites are found.")
        testFailWhenNoTestSuites
        (\v flags -> flags{testFailWhenNoTestSuites = v})
        trueArg
    , option
        []
        ["test-options"]
        ( "give extra options to test executables "
            ++ "(name templates can use $pkgid, $compiler, "
            ++ "$os, $arch, $test-suite)"
        )
        testOptions
        (\v flags -> flags{testOptions = v})
        ( reqArg'
            "TEMPLATES"
            (map toPathTemplate . splitArgs)
            (const [])
        )
    , option
        []
        ["test-option"]
        ( "give extra option to test executables "
            ++ "(no need to quote options containing spaces, "
            ++ "name template can use $pkgid, $compiler, "
            ++ "$os, $arch, $test-suite)"
        )
        testOptions
        (\v flags -> flags{testOptions = v})
        ( reqArg'
            "TEMPLATE"
            (\x -> [toPathTemplate x])
            (map fromPathTemplate)
        )
    ]

emptyTestFlags :: TestFlags
emptyTestFlags = mempty

instance Monoid TestFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup TestFlags where
  (<>) = gmappend
