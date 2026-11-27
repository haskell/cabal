-- | A test suite written against the @detailed-0.9@ interface, the way a
-- package author would write one: plain 'String' literals and no language
-- extensions.
--
-- Its purpose is to be compiled against the in-tree @Cabal@. The
-- @detailed-0.9@ packages under @cabal-testsuite/PackageTests@ are compiled
-- against the @Cabal@ that ships with GHC, so they do not notice a change to
-- "Distribution.TestSuite" that breaks test suites like this one.
module DetailedTests (tests) where

import Distribution.TestSuite

tests :: IO [Test]
tests =
  return
    [ Test passes
    , testGroup "group" [Test progresses]
    , Group
        { groupName = "sequential group"
        , concurrently = False
        , groupTests = [ExtraOptions [verbose] (Test configurable)]
        }
    ]

passes :: TestInstance
passes =
  TestInstance
    { run = return (Finished Pass)
    , name = "passes"
    , tags = ["fast"]
    , options = []
    , setOption = \_ _ -> Right passes
    }

progresses :: TestInstance
progresses =
  TestInstance
    { run = return (Progress "halfway" (return (Finished Pass)))
    , name = "progresses"
    , tags = []
    , options = []
    , setOption = \_ _ -> Right progresses
    }

configurable :: TestInstance
configurable =
  TestInstance
    { run = return (Finished Pass)
    , name = "configurable"
    , tags = ["fast", "options"]
    , options = [seed]
    , setOption = \option _ ->
        if option == optionName seed
          then Right configurable
          else Left ("unknown option: " ++ option)
    }

seed :: OptionDescr
seed =
  OptionDescr
    { optionName = "seed"
    , optionDescription = "Seed for the random number generator"
    , optionType = OptionRngSeed
    , optionDefault = Nothing
    }

verbose :: OptionDescr
verbose =
  OptionDescr
    { optionName = "verbose"
    , optionDescription = "Print more output"
    , optionType = OptionBool
    , optionDefault = Just "False"
    }
