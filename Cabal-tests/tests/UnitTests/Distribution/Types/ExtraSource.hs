module UnitTests.Distribution.Types.ExtraSource (extraSourceTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Distribution.Parsec                (eitherParsec)
import Distribution.Pretty
import Distribution.Types.ExtraSource
import Distribution.Utils.Path            (makeSymbolicPath)

import Test.QuickCheck.Instances.Cabal ()

extraSourceTests :: [TestTree]
extraSourceTests =
    [ testProperty "eitherParsec . prettyShow = Right" prop_parse_disp
    , testGroup "parse"  (map parseCase  parseCases)
    , testGroup "render" (map renderCase renderCases)
    , testGroup "reject" (map rejectCase rejectCases)
    ]

-- | The options are kept verbatim, so a round-trip only has to survive the
-- escaping that rendering applies to parentheses and backslashes.
--
-- Note this parses at 'cabalSpecLatest', which is at least the version that
-- introduced per-file options; below that they are rejected outright.
prop_parse_disp :: ExtraSource -> Property
prop_parse_disp es = counterexample (show (prettyShow es)) $
    eitherParsec (prettyShow es) === Right es

extraSource :: FilePath -> String -> ExtraSource
extraSource p = ExtraSource (makeSymbolicPath p)

-- | Everything between the parentheses is taken verbatim. Only @(@, @)@ and a
-- backslash in front of one of those (or of another backslash) has any
-- meaning; splitting into individual options happens later, at the use site.
parseCases :: [(String, ExtraSource)]
parseCases =
    [ ("a.c",                       extraSource "a.c" "")
    , ("a.c (-DFOO -O2)",           extraSource "a.c" "-DFOO -O2")
    , ("a.c (\"-DX=a b\")",         extraSource "a.c" "\"-DX=a b\"")
      -- A quote has no meaning here; splitArgs interprets it later.
    , ("a.c (-DX=\"a b\")",         extraSource "a.c" "-DX=\"a b\"")
      -- Parentheses may nest as long as they balance.
    , ("a.c (-DX=f(1))",            extraSource "a.c" "-DX=f(1)")
      -- An unbalanced parenthesis has to be escaped.
    , ("a.c (-DPAREN=\\))",         extraSource "a.c" "-DPAREN=)")
      -- A backslash not in front of '(', ')' or '\\' stands for itself, so
      -- Windows paths need no doubling.
    , ("a.c (-DPATH=C:\\foo\\bar)", extraSource "a.c" "-DPATH=C:\\foo\\bar")
    , ("a.c (\\\\)",                extraSource "a.c" "\\")
    ]

parseCase :: (String, ExtraSource) -> TestTree
parseCase (input, expected) =
    testCase (show input) $ eitherParsec input @?= Right expected

-- | Rendering escapes only when a verbatim copy would not be read back as
-- itself, so options that need no escaping survive unchanged.
renderCases :: [(ExtraSource, String)]
renderCases =
    [ (extraSource "a.c" "",                    "a.c")
    , (extraSource "a.c" "-DFOO -O2",           "a.c (-DFOO -O2)")
    , (extraSource "a.c" "-DX=f(1)",            "a.c (-DX=f(1))")
    , (extraSource "a.c" "-DPAREN=)",           "a.c (-DPAREN=\\))")
    , (extraSource "a.c" "-DPATH=C:\\foo\\bar", "a.c (-DPATH=C:\\foo\\bar)")
      -- Only the trailing backslash needs escaping: it would otherwise escape
      -- the closing parenthesis. The one inside the path is left alone.
    , (extraSource "a.c" "C:\\foo\\",           "a.c (C:\\foo\\\\)")
    ]

renderCase :: (ExtraSource, String) -> TestTree
renderCase (es, expected) =
    testCase (show expected) $ prettyShow es @?= expected

-- | An unbalanced parenthesis ends the options early, and quoting does not
-- protect it: the scan runs before any option lexing.
rejectCases :: [String]
rejectCases =
    [ "a.c (\"-DPAREN=)\")"
    , "a.c (-DFOO"
    ]

rejectCase :: String -> TestTree
rejectCase input = testCase (show input) $
    case eitherParsec input :: Either String ExtraSource of
        Left _   -> return ()
        Right es -> assertFailure ("expected a parse error, got " ++ show es)
