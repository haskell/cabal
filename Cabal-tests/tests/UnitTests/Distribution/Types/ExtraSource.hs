module UnitTests.Distribution.Types.ExtraSource (extraSourceTests) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Distribution.Parsec                (eitherParsec)
import Distribution.Pretty
import Distribution.Types.ExtraSource

import Test.QuickCheck.Instances.Cabal ()

extraSourceTests :: [TestTree]
extraSourceTests =
    [ testProperty "eitherParsec . prettyShow = Right" prop_parse_disp
    ]

-- | Per-file options may contain whitespace and parentheses, which
-- 'Distribution.Types.ExtraSource.showExtraSourceOpt' has to quote for
-- 'parsec' to read them back verbatim.
--
-- Note this parses at 'cabalSpecLatest', which is at least the version that
-- introduced per-file options; below that they are rejected outright.
prop_parse_disp :: ExtraSource -> Property
prop_parse_disp es = counterexample (show (prettyShow es)) $
    eitherParsec (prettyShow es) === Right es
