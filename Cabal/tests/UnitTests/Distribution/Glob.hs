module UnitTests.Distribution.Glob
    ( tests
    ) where

import Distribution.Glob
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

tests :: [Test]
tests =
    map toTest testData
    where
    toTest (input, expected) =
        testCase ("parseGlob " ++ input)
            (assertEqual "" expected (parseGlob input))

testData :: [(FilePath, Maybe Glob)]
testData =
    [ ("dictionary.txt", Just [Literal "dictionary.txt"])
    , ("test/*.hs", Just [Literal "test/", MatchAny, Literal ".hs"])
    , ("{unterminated,choice", Nothing)
    , ("{hello,goodbye}", Just [Choice
                            [[Literal "hello"], [Literal "goodbye"]]])
    , ("tests/**/*.hs", Just
        [ Literal "tests/"
        , MatchAnyRecursive
        , Literal "/"
        , MatchAny
        , Literal ".hs"
        ])
    ]
