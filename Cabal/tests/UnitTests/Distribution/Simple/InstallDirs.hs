module UnitTests.Distribution.Simple.InstallDirs
    ( tests
    ) where

import Distribution.Simple.InstallDirs
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck ((==>), Property)
import Test.HUnit.Base hiding (Test) --(assertBool, (@?=), (@=?))
--import Test.HUnit.Lang (Assertion)

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)

tests :: [Test]
tests =
  [ testCase "Parsing reversible" $ assert $ checkParsingReversible "foo-$os" "foo-asdassa"
  , testCase "Parsing reversible 2" $ assert $ checkParsingReversible "foo-$pkgid" "foo-asdassa-1.0"
  , testProperty "Parsing reversible" $ parsingReversibleProp
  ] ++ map testDatumToTest testData

type TestDatum = (TestName, String, FilePath, Maybe (PathTemplateEnv, String))

testDatumToTest :: TestDatum -> Test
testDatumToTest (msg, strPathTemplate, filePath, expectedResult) =
  testCase msg $ testPattern strPathTemplate filePath expectedResult

testData :: [TestDatum]
testData =
  [ ("empty pattern &  matching string", "", "", Just ([], ""))
  , ("empty pattern & !matching string", "", "a", Just ([], "a"))
  , ("literal pattern &  matching string", "foo", "foo", Just ([], ""))
  , ("literal pattern & !matching string", "foo", "bar", Nothing)
  , ("pattern with var 1 &  matching string", "foo$os", "foo-asdassa", Just ([(OSVar, toPathTemplate "-")], "asdassa"))
  , ("pattern with var 2 &  matching string", "foo$pkgid", "foo-asdassa", Just ([(PkgIdVar, toPathTemplate "-")], "asdassa"))
  , ("pattern with var 3 &  matching string", "foo$pkgid", "fooasdassa", Just ([(PkgIdVar, toPathTemplate "a")], "sdassa"))
  , ("pattern with var 4 &  matching string", "foo$os-bar", "foo-asdassa-bar", Just ([(OSVar, toPathTemplate "-asdassa")], ""))
  , ("pattern with var 4 & !matching string", "foo$os-bar", "foo-asdassa-ba", Nothing)
  ]

testPattern :: String -> FilePath -> Maybe (PathTemplateEnv, String) -> Assertion
testPattern strPathTemplate filePath expectedResult = do
--expectedParse expectedInputRest
  (compareAssocList `on` fmap fst) expectedResult actualResult
  ((@=?) `on` fmap snd) expectedResult actualResult
  where
    actualResult = parseTemplate (toPathTemplate strPathTemplate) filePath
--    actualParse = fmap fst actualResult
--    actualInputRest = fmap snd actualResult

compareAssocList :: (Ord a, Eq b, Show a, Show b) => Maybe [(a, b)] -> Maybe [(a, b)] -> Assertion
compareAssocList = (@=?) `on` fmap (sortBy (comparing fst))

-- 

-- | Check that pretty-printing the environment and the template returns the
-- given filePath.
parsingReversible :: PathTemplateEnv -> String -> PathTemplate -> FilePath -> Bool
parsingReversible env inputRest pathTemplate filePath =
  fromPathTemplate (substPathTemplate env pathTemplate) ++ inputRest == filePath

checkParsingReversible :: String -> FilePath -> Assertion
checkParsingReversible strPathTemplate filePath =
  case parseTemplateRes of
    Just (parsedEnv, inputRest) -> assert $ parsingReversible parsedEnv inputRest pathTemplate filePath
    Nothing -> assertFailure "Bug in test - parsing failed in checkParsingReversible"
  where
    pathTemplate = toPathTemplate strPathTemplate
    parseTemplateRes = parseTemplate pathTemplate filePath

parsingReversibleProp :: String -> FilePath -> Property
parsingReversibleProp strPathTemplate filePath =
  isJust parseTemplateRes ==>
    let (Just (parsedEnv, inputRest)) = parseTemplateRes in
    parsingReversible parsedEnv inputRest pathTemplate filePath
  where
    pathTemplate = toPathTemplate strPathTemplate
    parseTemplateRes = parseTemplate pathTemplate filePath
